{-# LANGUAGE
    NamedFieldPuns
  #-}

module LocalCooking.Function.Validate where

import LocalCooking.Function.System (SystemM, SystemEnv (..), getUserId, getSystemEnv, liftDb)
import LocalCooking.Database.Schema
  ( getChefId, getMenuId, getMealId, Unique (UniqueEmail), StoredUser (..))
import LocalCooking.Common.User.Password (HashedPassword)
import LocalCooking.Common.AccessToken.Auth (AuthToken)

import Data.Time.Calendar (Day)
import Data.Text.Permalink (Permalink)
import Text.EmailAddress (EmailAddress)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.Persist (Entity (..))
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Class (getBy, get)



uniqueEmail :: EmailAddress -> SystemM Bool
uniqueEmail e = do
  liftDb $ do
    mEnt <- getBy (UniqueEmail e)
    case mEnt of
      Nothing -> pure True
      Just _ -> pure False


uniqueChefPermalink :: Permalink -> SystemM Bool
uniqueChefPermalink p = do
  liftDb $ do
    mId <- getChefId p
    pure $ case mId of
      Nothing -> False
      Just _ -> True


uniqueMenuDeadline :: Permalink -> Day -> SystemM Bool
uniqueMenuDeadline p d = do
  liftDb $ do
    mId <- getMenuId p d
    pure $ case mId of
      Nothing -> False
      Just _ -> True


uniqueMealPermalink :: Permalink -> Day -> Permalink -> SystemM Bool
uniqueMealPermalink c d m = do
  liftDb $ do
    mId <- getMealId c d m
    pure $ case mId of
      Nothing -> False
      Just _ -> True


passwordVerify :: AuthToken -> HashedPassword -> SystemM Bool
passwordVerify authToken pw = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure False
    Just userId -> do
      liftDb $ do
        mUser <- get userId
        case mUser of
          Nothing -> pure False
          Just (StoredUser _ _ pw' _) -> pure (pw == pw')


passwordVerifyUnauth :: EmailAddress -> HashedPassword -> SystemM Bool
passwordVerifyUnauth email pw = do
  liftDb $ do
    mUserId <- getBy (UniqueEmail email)
    case mUserId of
      Nothing -> pure False
      Just (Entity _ (StoredUser _ _ pw' _)) -> pure (pw == pw')
