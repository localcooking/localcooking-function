{-# LANGUAGE
    NamedFieldPuns
  #-}

module LocalCooking.Function.Validate where

import LocalCooking.Function.System (AppM, SystemEnv (..), getUserId)
import LocalCooking.Database.Query.Semantics (getChefId, getMenuId, getMealId)
import LocalCooking.Database.Schema.User
  (Unique (UniqueEmail), StoredUser (..))
import LocalCooking.Common.User.Password (HashedPassword)
import LocalCooking.Common.AccessToken.Auth (AuthToken)

import Data.Time.Calendar (Day)
import Data.Text.Permalink (Permalink)
import Text.EmailAddress (EmailAddress)
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.Persist (Entity (..))
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Class (getBy, get)



uniqueEmail :: EmailAddress -> AppM Bool
uniqueEmail e = do
  SystemEnv{systemEnvDatabase} <- ask

  liftIO $ flip runSqlPool systemEnvDatabase $ do
    mEnt <- getBy (UniqueEmail e)
    case mEnt of
      Nothing -> pure True
      Just _ -> pure False


uniqueChefPermalink :: Permalink -> AppM Bool
uniqueChefPermalink p = do
  SystemEnv{systemEnvDatabase} <- ask

  mId <- liftIO (getChefId systemEnvDatabase p)
  pure $ case mId of
    Nothing -> False
    Just _ -> True


uniqueMenuDeadline :: Permalink -> Day -> AppM Bool
uniqueMenuDeadline p d = do
  SystemEnv{systemEnvDatabase} <- ask

  mId <- liftIO (getMenuId systemEnvDatabase p d)
  pure $ case mId of
    Nothing -> False
    Just _ -> True


uniqueMealPermalink :: Permalink -> Day -> Permalink -> AppM Bool
uniqueMealPermalink c d m = do
  SystemEnv{systemEnvDatabase} <- ask

  mId <- liftIO (getMealId systemEnvDatabase c d m)
  pure $ case mId of
    Nothing -> False
    Just _ -> True


passwordVerify :: AuthToken -> HashedPassword -> AppM Bool
passwordVerify authToken pw = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure False
    Just userId -> do
      SystemEnv{systemEnvDatabase} <- ask
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        mUser <- get userId
        case mUser of
          Nothing -> pure False
          Just (StoredUser _ _ pw' _) -> pure (pw == pw')


passwordVerifyUnauth :: EmailAddress -> HashedPassword -> AppM Bool
passwordVerifyUnauth email pw = do
  SystemEnv{systemEnvDatabase} <- ask
  liftIO $ flip runSqlPool systemEnvDatabase $ do
    mUserId <- getBy (UniqueEmail email)
    case mUserId of
      Nothing -> pure False
      Just (Entity _ (StoredUser _ _ pw' _)) -> pure (pw == pw')
