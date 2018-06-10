{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  #-}

module LocalCooking.Function.Admin where

import LocalCooking.Semantics.Common (User (..), SocialLoginForm (..), Register (..))
import LocalCooking.Function.System (SystemM, SystemEnv (..), getUserId, guardRole, getSystemEnv)
import LocalCooking.Database.Schema.Facebook.UserDetails (FacebookUserDetails (..), Unique (FacebookUserDetailsOwner))
import LocalCooking.Database.Schema.User
  ( StoredUser (..)
  , EntityField
    (StoredUserEmail, StoredUserCreated, StoredUserConfirmed)
  , Unique (UniqueEmail))
import LocalCooking.Database.Schema.User.Role (UserRoleStored (..), EntityField (UserRoleStoredUserRoleOwner))
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.User.Role (UserRole (Admin))

import Data.IORef (newIORef, readIORef, modifyIORef)
import qualified Data.Set as Set
import Data.Time (getCurrentTime)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.Persist (Entity (..), (==.), (=.))
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Class (selectList, getBy, insert, insert_, delete, deleteBy, update)



getUsers :: AuthToken -> SystemM (Maybe [User])
getUsers authToken = do
  isAuthorized <- verifyAdminhood authToken
  if not isAuthorized
    then pure Nothing
    else do
      SystemEnv{systemEnvDatabase} <- getSystemEnv

      liftIO $ flip runSqlPool systemEnvDatabase $ do
        xs <- selectList [] []
        fmap Just $ forM xs $ \(Entity k (StoredUser created email _ conf)) -> do
          roles <- fmap (fmap (\(Entity _ (UserRoleStored r _)) -> r))
                $ selectList [UserRoleStoredUserRoleOwner ==. k] []
          mFb <- getBy (FacebookUserDetailsOwner k)
          pure User
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



setUser :: AuthToken -> User -> SystemM Bool
setUser authToken User{..} = do
  isAuthorized <- verifyAdminhood authToken
  if not isAuthorized
    then pure False
    else do
      SystemEnv{systemEnvDatabase} <- getSystemEnv

      liftIO $ flip runSqlPool systemEnvDatabase $ do
        update userId
          [ StoredUserCreated =. userCreated
          , StoredUserEmail =. userEmail
          , StoredUserConfirmed =. userEmailConfirmed
          ]
        case userSocialLogin of
          SocialLoginForm mFb -> do
            case mFb of
              Nothing -> deleteBy (FacebookUserDetailsOwner userId)
              Just userFb -> insert_ (FacebookUserDetails userFb userId)
        xs <- selectList [UserRoleStoredUserRoleOwner ==. userId] []
        rolesRef <- liftIO (newIORef (Set.fromList userRoles))
        forM_ xs $ \(Entity k (UserRoleStored role _)) -> do
          roles' <- liftIO (readIORef rolesRef)
          if Set.member role roles'
            then liftIO $ modifyIORef rolesRef $ Set.delete role
            else delete k
        rolesLeft <- liftIO (readIORef rolesRef)
        forM_ rolesLeft $ \role -> insert_ (UserRoleStored role userId)

        pure True



addUser :: AuthToken -> Register -> SystemM Bool
addUser authToken Register{..} = do
  isAuthorized <- verifyAdminhood authToken
  if not isAuthorized
    then pure False
    else do
      SystemEnv{systemEnvDatabase} <- getSystemEnv

      liftIO $ flip runSqlPool systemEnvDatabase $ do
        mEnt <- getBy (UniqueEmail registerEmail)
        case mEnt of
          Just _ -> pure False
          Nothing -> do
            now <- liftIO getCurrentTime
            k <- insert (StoredUser now registerEmail registerPassword False)
            case registerSocial of
              SocialLoginForm mFb -> do
                case mFb of
                  Nothing -> pure ()
                  Just userFb -> insert_ (FacebookUserDetails userFb k)
            pure True



verifyAdminhood :: AuthToken -> SystemM Bool
verifyAdminhood authToken = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure False
    Just userId -> guardRole userId Admin
