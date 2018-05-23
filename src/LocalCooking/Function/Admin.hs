{-# LANGUAGE
    NamedFieldPuns
  #-}

module LocalCooking.Function.Admin where

import LocalCooking.Semantics.Admin (GetUsers (..))
import LocalCooking.Semantics.Common (User (..), SocialLoginForm (..))
import LocalCooking.Function.System (AppM, SystemEnv (..))
import LocalCooking.Database.Schema.Facebook.UserDetails (FacebookUserDetails (..), Unique (FacebookUserDetailsOwner))
import LocalCooking.Database.Schema.User (StoredUser (..))
import LocalCooking.Database.Schema.User.Role (UserRoleStored (..), EntityField (UserRoleStoredUserRoleOwner))
import LocalCooking.Database.Schema.User.Pending (Unique (UniquePendingRegistration))

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import Database.Persist (Entity (..), (==.))
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Class (selectList, getBy)



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
