{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  , OverloadedStrings
  #-}

module LocalCooking.Function.Common where

import LocalCooking.Semantics.Common
  (User (..), Login (..), SocialLoginForm (..), Register (..), SocialLogin (..))
import LocalCooking.Function.System
  (AppM, SystemEnv (..), TokenContexts (..), Managers (..), Keys (..))
import LocalCooking.Function.System.AccessToken (insertAccess, lookupAccess, revokeAccess)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Database.Schema.Facebook.UserDetails (Unique (FacebookUserDetailsOwner))
import LocalCooking.Database.Schema.User
  ( StoredUser (..), StoredUserId
  , EntityField
    (StoredUserEmail, StoredUserCreated, StoredUserConfirmed)
  , Unique (UniqueEmail))
import LocalCooking.Database.Schema.Facebook.AccessToken
  ( FacebookUserAccessTokenStored (..)
  )
import LocalCooking.Database.Schema.Facebook.UserDetails
  ( FacebookUserDetails (..)
  , Unique (UniqueFacebookUserId)
  )
import LocalCooking.Database.Schema.User.Role (UserRoleStored (..), EntityField (UserRoleStoredUserRoleOwner))
import LocalCooking.Common.AccessToken.Email (EmailToken)
import SparkPost.Keys (SparkPostCredentials (..), confirmEmailRequest)
import Facebook.Return (FacebookLoginReturnError, handleFacebookLoginReturn)
import Google.Keys
  ( GoogleCredentials (..), ReCaptchaSecret (getReCaptchaSecret)
  , ReCaptchaResponse (getReCaptchaResponse), ReCaptchaVerifyResponse (..)
  , googleReCaptchaVerifyURI)

import Data.URI (printURI)
import Data.IORef (newIORef, readIORef, modifyIORef)
import qualified Data.Set as Set
import Data.Time (getCurrentTime)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as Aeson
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import Control.Concurrent.STM (atomically)
import Database.Persist (Entity (..), (==.), (=.))
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Class (selectList, getBy, insert, insert_, delete, deleteBy, update, get)
import Network.HTTP.Client (httpLbs, method, responseBody, urlEncodedBody, parseRequest)



newEmailToken :: StoredUserId -> AppM EmailToken
newEmailToken userId = do
  SystemEnv{systemEnvTokenContexts} <- ask

  case systemEnvTokenContexts of
    TokenContexts{tokenContextEmail} -> do
      token <- liftIO (insertAccess tokenContextEmail userId)
      pure token


confirmEmail :: EmailToken -> AppM Bool
confirmEmail token = do
  SystemEnv{systemEnvTokenContexts,systemEnvDatabase} <- ask

  case systemEnvTokenContexts of
    TokenContexts{tokenContextEmail} -> do
      mUserId <- liftIO (lookupAccess tokenContextEmail token)
      case mUserId of
        Nothing -> pure False
        Just userId -> do
          liftIO $ flip runSqlPool systemEnvDatabase $ do
            -- FIXME lightweight existence checker?
            mUser <- get userId
            case mUser of
              Nothing -> pure False
              Just _ -> do
                liftIO $ atomically $ revokeAccess tokenContextEmail token
                update userId [StoredUserConfirmed =. True]
                pure True


login :: Login -> AppM (Maybe AuthToken)
login Login{..} = do
  SystemEnv{systemEnvDatabase,systemEnvTokenContexts} <- ask

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
            -> AppM (Either (Maybe FacebookLoginReturnError) AuthToken)
socialLogin x = case x of
  SocialLoginFB{..} -> do
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
      } <- ask
    --  FIXME needs a HTTP reach-around for FB login ><
    eLoginErr <- liftIO $ handleFacebookLoginReturn
      managersFacebook
      keysFacebook
      systemEnvFBRedirect
      socialLoginFBCode
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


register :: Register -> AppM Bool
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
    } <- ask

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
      pure False
    Just (ReCaptchaVerifyResponse success)
      | not success -> do
        liftIO $ putStrLn $ "recaptcha failure: " ++ show (responseBody resp)
        pure False
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
          Nothing -> pure False
          Just userId -> do
            emailToken <- newEmailToken userId
            req <- liftIO (confirmEmailRequest sparkPostKey registerEmail emailToken)
            resp <- liftIO (httpLbs req managersSparkPost)
            liftIO $ do
              putStrLn $ "Sent email confirmation:"
              putStrLn $ show req
              putStrLn $ show resp
            -- FIXME TODO note - the redirect URI is defined in the spark post template
            pure True


-- | Something like "get user details"
getUser :: AuthToken -> AppM (Maybe User)
getUser authToken = do
  SystemEnv{systemEnvTokenContexts,systemEnvDatabase} <- ask

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
                  , userSocial = SocialLoginForm
                    { socialLoginFormFb = case mFb of
                      Nothing -> Nothing
                      Just (Entity _ (FacebookUserDetails uId _)) -> Just uId
                    }
                  , userEmailConfirmed = conf
                  , userRoles = roles
                  }


-- FIXME TODO changePassword?
-- | "set user details"
setUser :: AuthToken -> User -> AppM Bool
setUser authToken User{..} = do
  SystemEnv{systemEnvTokenContexts,systemEnvDatabase} <- ask

  case systemEnvTokenContexts of
    TokenContexts{tokenContextAuth} -> do
      mUserId <- liftIO (lookupAccess tokenContextAuth authToken)
      case mUserId of
        Nothing -> pure False
        Just k
          | k /= userId -> pure False
          | otherwise ->
              liftIO $ flip runSqlPool systemEnvDatabase $ do
                update userId
                  [ StoredUserCreated =. userCreated
                  , StoredUserEmail =. userEmail
                  , StoredUserConfirmed =. userEmailConfirmed
                  ]
                case userSocial of
                  SocialLoginForm mFb -> do
                    case mFb of
                      Nothing -> deleteBy (FacebookUserDetailsOwner userId)
                      Just userFb -> insert_ (FacebookUserDetails userFb userId)
                xs <- selectList [UserRoleStoredUserRoleOwner ==. userId] []
                rolesRef <- liftIO (newIORef (Set.fromList userRoles))
                forM_ xs $ \(Entity roleId (UserRoleStored role _)) -> do
                  roles' <- liftIO (readIORef rolesRef)
                  if Set.member role roles'
                    then liftIO $ modifyIORef rolesRef $ Set.delete role
                    else delete roleId
                rolesLeft <- liftIO (readIORef rolesRef)
                forM_ rolesLeft $ \role -> insert_ (UserRoleStored role userId)
                pure True


-- TODO FIXME individual field validation
