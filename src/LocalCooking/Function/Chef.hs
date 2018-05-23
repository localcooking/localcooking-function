{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  #-}

module LocalCooking.Function.Chef where

import LocalCooking.Function.System (AppM, SystemEnv (..), TokenContexts (..))
import LocalCooking.Function.System.AccessToken (lookupAccess)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.User.Role (UserRole (Chef))
import LocalCooking.Database.Query.Semantics.Admin (hasRole)
import LocalCooking.Database.Query.Tag.Meal (insertMealTag)
import LocalCooking.Database.Query.Tag.Chef (insertChefTag)

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import Database.Persist (Entity (..), (==.), (=.))
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Class (selectList, getBy, insert, insert_, delete, deleteBy, update, get)



addMealTag :: AuthToken -> MealTag -> AppM Bool
addMealTag authToken tag = do
  SystemEnv{systemEnvTokenContexts,systemEnvDatabase} <- ask

  case systemEnvTokenContexts of
    TokenContexts{tokenContextAuth} -> do
      mUserId <- liftIO (lookupAccess tokenContextAuth authToken)
      case mUserId of
        Nothing -> pure False
        Just k -> do
          isAuthorized <- liftIO (hasRole systemEnvDatabase k Chef)
          if not isAuthorized
            then pure False
            else True <$ liftIO (insertMealTag systemEnvDatabase tag)


addChefTag :: AuthToken -> ChefTag -> AppM Bool
addChefTag authToken tag = do
  SystemEnv{systemEnvTokenContexts,systemEnvDatabase} <- ask

  case systemEnvTokenContexts of
    TokenContexts{tokenContextAuth} -> do
      mUserId <- liftIO (lookupAccess tokenContextAuth authToken)
      case mUserId of
        Nothing -> pure False
        Just k -> do
          isAuthorized <- liftIO (hasRole systemEnvDatabase k Chef)
          if not isAuthorized
            then pure False
            else True <$ liftIO (insertChefTag systemEnvDatabase tag)
