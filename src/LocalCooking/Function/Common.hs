{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  #-}

module LocalCooking.Function.Common where

import LocalCooking.Semantics.Admin (GetUsers (..), SetUser (..), AddUser (..))
import LocalCooking.Semantics.Common (Login (..), SocialLoginForm (..), Register (..))
import LocalCooking.Function.System (AppM, SystemEnv (..), TokenContexts (..))
import LocalCooking.Function.System.AccessToken (insertAccess)
import LocalCooking.Common.AccessToken (genAccessToken)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Database.Schema.Facebook.UserDetails (FacebookUserDetails (..), Unique (FacebookUserDetailsOwner))
import LocalCooking.Database.Schema.User (StoredUser (..), EntityField (StoredUserEmail, StoredUserPassword, StoredUserCreated), Unique (UniqueEmail))
import LocalCooking.Database.Schema.User.Role (UserRoleStored (..), EntityField (UserRoleStoredUserRoleOwner))
import LocalCooking.Database.Schema.User.Pending (PendingRegistrationConfirm (..), Unique (UniquePendingRegistration))

import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import qualified Data.Set as Set
import Data.Time (getCurrentTime)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import Control.Newtype (Newtype (pack))
import Database.Persist (Entity (..), (==.), (=.))
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Class (selectList, getBy, insert, insert_, delete, deleteBy, update)



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
