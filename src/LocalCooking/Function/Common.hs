{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  , OverloadedStrings
  #-}

module LocalCooking.Function.Common where

import LocalCooking.Semantics.Common
  (User (..), SetUser (..), Login (..), SocialLoginForm (..), Register (..), RegisterError (..), ConfirmEmailError (..), SocialLogin (..))
import LocalCooking.Function.System
  (SystemM, SystemEnv (..), TokenContexts (..), Managers (..), Keys (..), getSystemEnv)
import LocalCooking.Function.System.AccessToken (insertAccess, lookupAccess, revokeAccess)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.AccessToken.Email (EmailToken)
import LocalCooking.Database.Schema
  ( Unique
    ( FacebookUserDetailsOwner, UniqueEmail, UniqueFacebookUserId
    )
  , EntityField
    ( StoredUserEmail, StoredUserCreated, StoredUserConfirmed, StoredUserPassword
    , UserRoleStoredUserRoleOwner
    )
  , StoredUser (..), StoredUserId
  , FacebookUserAccessTokenStored (..)
  , FacebookUserDetails (..)
  , UserRoleStored (..)
  )
import SparkPost.Keys (SparkPostCredentials (..), confirmEmailRequest)
import Facebook.Return (FacebookLoginReturnError, handleFacebookLoginReturn)
import Google.Keys
  ( GoogleCredentials (..), ReCaptchaSecret (getReCaptchaSecret)
  , ReCaptchaResponse (getReCaptchaResponse), ReCaptchaVerifyResponse (..)
  , googleReCaptchaVerifyURI)

import Data.URI (printURI)
import Data.IORef (newIORef, readIORef, modifyIORef)
import qualified Data.Set as Set
import Data.Monoid ((<>))
import Data.Time (getCurrentTime)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as Aeson
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Concurrent.STM (atomically)
import Control.Logging (log')
import Database.Persist (Entity (..), (==.), (=.))
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Class (selectList, getBy, insert, insert_, delete, deleteBy, update, get)
import Network.HTTP.Client (httpLbs, method, responseBody, urlEncodedBody, parseRequest)



newEmailToken :: StoredUserId -> SystemM EmailToken
newEmailToken userId = do
  SystemEnv{systemEnvTokenContexts} <- getSystemEnv

  case systemEnvTokenContexts of
    TokenContexts{tokenContextEmail} -> do
      token <- liftIO (insertAccess tokenContextEmail userId)
      pure token


confirmEmail :: EmailToken -> SystemM ConfirmEmailError
confirmEmail token = do
  SystemEnv{systemEnvTokenContexts,systemEnvDatabase} <- getSystemEnv

  case systemEnvTokenContexts of
    TokenContexts{tokenContextEmail} -> do
      mUserId <- liftIO (lookupAccess tokenContextEmail token)
      case mUserId of
        Nothing -> pure ConfirmEmailTokenNonexistent
        Just userId -> do
          liftIO $ flip runSqlPool systemEnvDatabase $ do
            -- FIXME lightweight existence checker?
            mUser <- get userId
            case mUser of
              Nothing -> pure ConfirmEmailUserNonexistent
              Just _ -> do
                liftIO $ atomically $ revokeAccess tokenContextEmail token
                update userId [StoredUserConfirmed =. True]
                pure ConfirmEmailOk


login :: Login -> SystemM (Maybe AuthToken)
login Login{..} = do
  SystemEnv{systemEnvDatabase,systemEnvTokenContexts} <- getSystemEnv

  mUserId <- liftIO $ flip runSqlPool systemEnvDatabase $ do
    mEnt <- getBy (UniqueEmail loginEmail)
    case mEnt of
      Nothing -> pure Nothing
      Just (Entity k (StoredUser _ _ p _))
        | p == loginPassword -> pure (Just k)
        | otherwise -> pure Nothing
  case mUserId of
    Nothing -> pure Nothing
    Just userId -> do
      case systemEnvTokenContexts of
        TokenContexts{tokenContextAuth} -> do
          token <- liftIO (insertAccess tokenContextAuth userId)
          pure (Just token)


-- | If there's no error, the associated userId with the facebookUserId doesn't exist.
socialLogin :: SocialLogin
            -> SystemM (Either (Maybe FacebookLoginReturnError) AuthToken)
socialLogin x = case x of
  SocialLoginFB{socialLoginFB} -> do
    SystemEnv
      { systemEnvManagers = Managers
        { managersFacebook
        }
      , systemEnvKeys = Keys
        { keysFacebook
        }
      , systemEnvFBRedirect
      , systemEnvDatabase
      , systemEnvTokenContexts = TokenContexts
        { tokenContextAuth
        }
      } <- getSystemEnv
    --  FIXME needs a HTTP reach-around for FB login ><
    eLoginErr <- liftIO $ handleFacebookLoginReturn
      managersFacebook
      keysFacebook
      systemEnvFBRedirect
      socialLoginFB
    case eLoginErr of
      Left e -> pure $ Left $ Just e
      Right (fbToken,fbUserId) -> do
        mUserId <- liftIO $ flip runSqlPool systemEnvDatabase $ do
          mFbUser <- getBy (UniqueFacebookUserId fbUserId)
          case mFbUser of
            Nothing -> pure Nothing
            Just (Entity fbDetailsId (FacebookUserDetails _ userId)) -> do
              insert_ (FacebookUserAccessTokenStored fbToken fbDetailsId)
              pure (Just userId)
        case mUserId of
          Nothing -> pure (Left Nothing)
          Just userId -> do
            token <- liftIO (insertAccess tokenContextAuth userId)
            pure (Right token)


-- FIXME register errors
register :: Register -> SystemM (Maybe RegisterError)
register Register{..} = do
  SystemEnv
    { systemEnvDatabase
    , systemEnvKeys = Keys
      { keysSparkPost = SparkPostCredentials{sparkPostKey}
      , keysGoogle = GoogleCredentials
        { googleReCaptchaSecret
        }
      }
    , systemEnvManagers = Managers
      { managersSparkPost
      , managersReCaptcha
      }
    } <- getSystemEnv

  resp <- liftIO $ do
    req <- parseRequest $ T.unpack $ printURI googleReCaptchaVerifyURI
    let req' = urlEncodedBody
          [ ("secret", T.encodeUtf8 $ getReCaptchaSecret googleReCaptchaSecret)
          , ("response", T.encodeUtf8 $ getReCaptchaResponse registerReCaptcha)
          ] ( req
              { method = "POST"
              }
            )
    httpLbs req' managersReCaptcha

  case Aeson.decode (responseBody resp) of
    Nothing -> do
      liftIO $ putStrLn $ "Couldn't parse response body: " ++ show (responseBody resp)
      pure $ Just $ RegisterDecodingError (responseBody resp)
    Just (ReCaptchaVerifyResponse success)
      | not success -> do
        liftIO $ putStrLn $ "recaptcha failure: " ++ show (responseBody resp)
        pure $ Just $ RegisterReCaptchaFailure (responseBody resp)
      | otherwise -> do
        mUserId <- liftIO $ flip runSqlPool systemEnvDatabase $ do
          mEnt <- getBy (UniqueEmail registerEmail)
          case mEnt of
            Just _ -> pure Nothing
            Nothing -> do
              now <- liftIO getCurrentTime
              userId <- insert (StoredUser now registerEmail registerPassword False)
              case registerSocial of
                SocialLoginForm mFb -> do
                  case mFb of
                    Nothing -> pure ()
                    Just userFb -> insert_ (FacebookUserDetails userFb userId)
              pure (Just userId)
        case mUserId of
          Nothing -> pure $ Just RegisterEmailTaken
          Just userId -> do
            log' "Registered user!"
            emailToken <- newEmailToken userId
            req <- liftIO (confirmEmailRequest sparkPostKey registerEmail emailToken)
            resp <- liftIO (httpLbs req managersSparkPost)
            log' $ "Sent email confirmation: " <> T.pack (show req) <> ", " <> T.pack (show resp)
            -- FIXME TODO note - the redirect URI is defined in the spark post template
            pure Nothing


-- | Something like "get user details"
getUser :: AuthToken -> SystemM (Maybe User)
getUser authToken = do
  SystemEnv{systemEnvTokenContexts,systemEnvDatabase} <- getSystemEnv

  case systemEnvTokenContexts of
    TokenContexts{tokenContextAuth} -> do
      mUserId <- liftIO (lookupAccess tokenContextAuth authToken)
      case mUserId of
        Nothing -> pure Nothing
        Just k -> do
          liftIO $ flip runSqlPool systemEnvDatabase $ do
            mStoredUser <- get k
            case mStoredUser of
              Nothing -> pure Nothing
              Just (StoredUser created email _ conf) -> do
                roles <- fmap (fmap (\(Entity _ (UserRoleStored r _)) -> r))
                      $ selectList [UserRoleStoredUserRoleOwner ==. k] []
                mFb <- getBy (FacebookUserDetailsOwner k)
                pure $ Just User
                  { userId = k
                  , userCreated = created
                  , userEmail = email
                  , userSocialLogin = SocialLoginForm
                    { socialLoginFormFb = case mFb of
                      Nothing -> Nothing
                      Just (Entity _ (FacebookUserDetails uId _)) -> Just uId
                    }
                  , userEmailConfirmed = conf
                  , userRoles = roles
                  }


-- FIXME TODO changePassword? Better error symbols?
-- | "set user details"
setUser :: AuthToken -> SetUser -> SystemM Bool
setUser authToken SetUser{..} = do
  SystemEnv{systemEnvTokenContexts,systemEnvDatabase} <- getSystemEnv

  case systemEnvTokenContexts of
    TokenContexts{tokenContextAuth} -> do
      mUserId <- liftIO (lookupAccess tokenContextAuth authToken)
      case mUserId of
        Nothing -> pure False
        Just k
          | k /= setUserId -> pure False
          | otherwise ->
              liftIO $ flip runSqlPool systemEnvDatabase $ do
                mUser <- get setUserId
                case mUser of
                  Nothing -> pure False
                  Just (StoredUser _ _ pw _)
                    | pw /= setUserOldPassword -> pure False
                    | otherwise -> do
                      update setUserId
                        [ StoredUserEmail =. setUserEmail
                        , StoredUserPassword =. setUserNewPassword
                        ]
                      case setUserSocialLogin of
                        SocialLoginForm mFb -> do
                          case mFb of
                            Nothing -> deleteBy (FacebookUserDetailsOwner setUserId)
                            Just userFb -> insert_ (FacebookUserDetails userFb setUserId)
                      pure True


-- TODO FIXME individual field validation
