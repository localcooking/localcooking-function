{-# LANGUAGE
    NamedFieldPuns
  #-}

module LocalCooking.Function.Validate where

import LocalCooking.Function.System (AppM, SystemEnv (..))
import LocalCooking.Database.Schema.User (Unique (UniqueEmail))

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
