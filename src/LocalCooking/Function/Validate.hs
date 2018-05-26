{-# LANGUAGE
    NamedFieldPuns
  #-}

module LocalCooking.Function.Validate where

import LocalCooking.Function.System (AppM, SystemEnv (..))
import LocalCooking.Database.Query.Semantics (getChefId, getMenuId, getMealId)
import LocalCooking.Database.Schema.User
  (Unique (UniqueEmail))

import Data.Time.Calendar (Day)
import Data.Text.Permalink (Permalink)
import Text.EmailAddress (EmailAddress)
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Class (getBy)



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
