{-# LANGUAGE
    NamedFieldPuns
  #-}

module LocalCooking.Function.Validate where

import LocalCooking.Function.User (getUserId)
import LocalCooking.Function.System (SystemM, SystemEnv (..), getSystemEnv, liftDb)
import LocalCooking.Semantics.User (UserExists (..))
import LocalCooking.Database.Schema
  ( getChefId, getMenuId, getMealId, Unique (UniqueEmail), StoredUser (..))
import LocalCooking.Common.User.Password (HashedPassword)
import LocalCooking.Common.AccessToken.Auth (AuthToken)

import Data.Time.Calendar (Day)
import Data.Text.Permalink (Permalink)
import Text.EmailAddress (EmailAddress)
import Control.Monad.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.Persist (Entity (..))
import Database.Persist.Sql (runSqlPool, SqlBackend)
import Database.Persist.Class (getBy, get)



uniqueEmail :: EmailAddress -> ReaderT SqlBackend IO Bool
uniqueEmail e = do
  mEnt <- getBy (UniqueEmail e)
  case mEnt of
    Nothing -> pure True
    Just _ -> pure False


uniqueChefPermalink :: Permalink -> ReaderT SqlBackend IO Bool
uniqueChefPermalink p = do
  mId <- getChefId p
  pure $ case mId of
    Nothing -> False
    Just _ -> True


uniqueMenuDeadline :: Permalink -> Day -> ReaderT SqlBackend IO Bool
uniqueMenuDeadline p d = do
  mId <- getMenuId p d
  pure $ case mId of
    Nothing -> False
    Just _ -> True


uniqueMealPermalink :: Permalink -> Day -> Permalink -> ReaderT SqlBackend IO Bool
uniqueMealPermalink c d m = do
  mId <- getMealId c d m
  pure $ case mId of
    Nothing -> False
    Just _ -> True


passwordVerify :: AuthToken -> HashedPassword -> SystemM (UserExists Bool)
passwordVerify authToken pw = do
  mUserId <- getUserId authToken
  case mUserId of
    UserDoesntExist -> pure UserDoesntExist
    UserExists userId -> liftDb $ do
      mUser <- get userId
      case mUser of
        Nothing -> pure UserDoesntExist
        Just (StoredUser _ _ pw' _) -> pure (UserExists (pw == pw'))


passwordVerifyUnauth :: EmailAddress -> HashedPassword -> ReaderT SqlBackend IO (UserExists Bool)
passwordVerifyUnauth email pw = do
  mUserId <- getBy (UniqueEmail email)
  case mUserId of
    Nothing -> pure UserDoesntExist
    Just (Entity _ (StoredUser _ _ pw' _)) -> pure (UserExists (pw == pw'))
