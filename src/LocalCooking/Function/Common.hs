{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  #-}

module LocalCooking.Function.Common where

import LocalCooking.Semantics.Common (User (..), Login (..), SocialLoginForm (..), Register (..))
import LocalCooking.Function.System (AppM, SystemEnv (..), TokenContexts (..))
import LocalCooking.Function.System.AccessToken (insertAccess, lookupAccess)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Database.Schema.Facebook.UserDetails (FacebookUserDetails (..), Unique (FacebookUserDetailsOwner))
import LocalCooking.Database.Schema.User (StoredUser (..), EntityField (StoredUserEmail, StoredUserPassword, StoredUserCreated), Unique (UniqueEmail))
import LocalCooking.Database.Schema.User.Role (UserRoleStored (..), EntityField (UserRoleStoredUserRoleOwner))
import LocalCooking.Database.Schema.User.Pending (PendingRegistrationConfirm (..), Unique (UniquePendingRegistration))

import Data.IORef (newIORef, readIORef, modifyIORef)
import qualified Data.Set as Set
import Data.Time (getCurrentTime)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import Database.Persist (Entity (..), (==.), (=.))
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Class (selectList, getBy, insert, insert_, delete, deleteBy, update, get)



login :: Login -> AppM (Maybe AuthToken)
login Login{..} = do
  SystemEnv{systemEnvDatabase,systemEnvTokenContexts} <- ask

  mUserId <- liftIO $ flip runSqlPool systemEnvDatabase $ do
    mEnt <- getBy (UniqueEmail loginEmail)
    case mEnt of
      Nothing -> pure Nothing
      Just (Entity k (StoredUser _ _ p))
        | p == loginPassword -> pure (Just k)
        | otherwise -> pure Nothing
  case mUserId of
    Nothing -> pure Nothing
    Just userId -> do
      case systemEnvTokenContexts of
        TokenContexts{tokenContextAuth} -> do
          token <- liftIO (insertAccess tokenContextAuth userId)
          pure (Just token)


-- socialLogin :: SocialLogin -> AppM (Maybe AuthToken)
-- socialLogin socialLogin = case socialLogin of
--   SocialLoginFB{..} -> do
    -- FIXME needs a HTTP reach-around for FB login ><


register :: Register -> AppM Bool
register Register{..} = do
  SystemEnv{systemEnvDatabase} <- ask

  liftIO $ flip runSqlPool systemEnvDatabase $ do
    mEnt <- getBy (UniqueEmail registerEmail)
    case mEnt of
      Just _ -> pure False
      Nothing -> do
        now <- liftIO getCurrentTime
        k <- insert (StoredUser now registerEmail registerPassword)
        case registerSocial of
          SocialLoginForm mFb -> do
            case mFb of
              Nothing -> pure ()
              Just userFb -> insert_ (FacebookUserDetails userFb k)
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
              Just (StoredUser created email password) -> do
                mPending <- getBy (UniquePendingRegistration k)
                roles <- fmap (fmap (\(Entity _ (UserRoleStored r _)) -> r))
                      $ selectList [UserRoleStoredUserRoleOwner ==. k] []
                mFb <- getBy (FacebookUserDetailsOwner k)
                pure $ Just User
                  { userId = k
                  , userCreated = created
                  , userEmail = email
                  , userPassword = password
                  , userSocial = SocialLoginForm
                    { socialLoginFormFb = case mFb of
                      Nothing -> Nothing
                      Just (Entity _ (FacebookUserDetails uId _)) -> Just uId
                    }
                  , userEmailConfirmed = case mPending of
                      Nothing -> True
                      Just _ -> False
                  , userRoles = roles
                  }


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
                  , StoredUserPassword =. userPassword
                  ]
                if userEmailConfirmed
                  then deleteBy (UniquePendingRegistration userId)
                  else insert_ (PendingRegistrationConfirm userId)
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
