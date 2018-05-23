{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  #-}

module LocalCooking.Function.Admin where

import LocalCooking.Semantics.Admin (GetUsers (..), SetUser (..))
import LocalCooking.Semantics.Common (User (..), SocialLoginForm (..))
import LocalCooking.Function.System (AppM, SystemEnv (..))
import LocalCooking.Database.Schema.Facebook.UserDetails (FacebookUserDetails (..), Unique (FacebookUserDetailsOwner))
import LocalCooking.Database.Schema.User (StoredUser (..), EntityField (StoredUserEmail, StoredUserPassword, StoredUserCreated))
import LocalCooking.Database.Schema.User.Role (UserRoleStored (..), EntityField (UserRoleStoredUserRoleOwner))
import LocalCooking.Database.Schema.User.Pending (PendingRegistrationConfirm (..), Unique (UniquePendingRegistration))

import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import qualified Data.Set as Set
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import Database.Persist (Entity (..), (==.), (=.))
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Class (selectList, getBy, insert_, delete, deleteBy, update)



getUsers :: GetUsers -> AppM [User]
getUsers GetUsers = do
  SystemEnv{systemEnvDatabase} <- ask

  liftIO $ flip runSqlPool systemEnvDatabase $ do
    xs <- selectList [] []
    forM xs $ \(Entity k (StoredUser created email password)) -> do
      mPending <- getBy (UniquePendingRegistration k)
      roles <- fmap (fmap (\(Entity _ (UserRoleStored r _)) -> r))
             $ selectList [UserRoleStoredUserRoleOwner ==. k] []
      mFb <- getBy (FacebookUserDetailsOwner k)
      pure User
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



setUser :: SetUser -> AppM ()
setUser (SetUser User{..}) = do
  SystemEnv{systemEnvDatabase} <- ask

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
    forM_ xs $ \(Entity k (UserRoleStored role _)) -> do
      roles' <- liftIO (readIORef rolesRef)
      if Set.member role roles'
        then liftIO $ modifyIORef rolesRef $ Set.delete role
        else delete k
    rolesLeft <- liftIO (readIORef rolesRef)
    forM_ rolesLeft $ \role -> insert_ (UserRoleStored role userId)
