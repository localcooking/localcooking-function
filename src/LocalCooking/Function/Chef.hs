{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  #-}

module LocalCooking.Function.Chef where

import LocalCooking.Semantics.Chef (ChefSettings (..))
import LocalCooking.Function.System (AppM, SystemEnv (..), TokenContexts (..))
import LocalCooking.Function.System.AccessToken (lookupAccess)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.User.Role (UserRole (Chef))
import LocalCooking.Database.Schema.Semantics
  ( StoredChef (..), ChefTagRelation (..), StoredChefId
  , EntityField
    ( StoredChefStoredChefName, StoredChefStoredChefPermalink
    , StoredChefStoredChefImages, StoredChefStoredChefAvatar
    , StoredChefStoredChefBio
    , ChefTagRelationChefTagChef, ChefTagRelationChefTagChefTag
    )
  , Unique (UniqueChefOwner))
import LocalCooking.Database.Query.Semantics.Admin (hasRole)
import LocalCooking.Database.Query.Tag.Meal (insertMealTag)
import LocalCooking.Database.Query.Tag.Chef (insertChefTag, getChefTagId)

import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Control.Monad (forM_, forM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import Database.Persist (Entity (..), (==.), (=.))
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Class (selectList, getBy, insert, insert_, delete, deleteBy, deleteWhere, update, get)



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



setChef :: AuthToken -> ChefSettings -> AppM (Maybe StoredChefId)
setChef authToken ChefSettings{..} = do
  SystemEnv{systemEnvTokenContexts,systemEnvDatabase} <- ask

  case systemEnvTokenContexts of
    TokenContexts{tokenContextAuth} -> do
      mUserId <- liftIO (lookupAccess tokenContextAuth authToken)
      case mUserId of
        Nothing -> pure Nothing
        Just k -> do
          isAuthorized <- liftIO (hasRole systemEnvDatabase k Chef)
          if not isAuthorized
            then pure Nothing
            else flip runSqlPool systemEnvDatabase $ do
              mChefEnt <- getBy (UniqueChefOwner k)
              case mChefEnt of
                Nothing -> do
                  chefId <- insert $ StoredChef
                    k
                    chefSettingsName
                    chefSettingsPermalink
                    chefSettingsBio
                    chefSettingsImages
                    chefSettingsAvatar
                  forM_ chefSettingsTags $ \t -> do
                    mChefTagId <- liftIO (getChefTagId systemEnvDatabase t)
                    case mChefTagId of
                      Nothing -> pure ()
                      Just chefTagId -> insert_ (ChefTagRelation chefId chefTagId)
                  pure (Just chefId)
                Just (Entity chefId _) -> do
                  update chefId
                    [ StoredChefStoredChefName =. chefSettingsName
                    , StoredChefStoredChefPermalink =. chefSettingsPermalink
                    , StoredChefStoredChefBio =. chefSettingsBio
                    , StoredChefStoredChefImages =. chefSettingsImages
                    , StoredChefStoredChefAvatar =. chefSettingsAvatar
                    ]
                  oldTags <- fmap (fmap (\(Entity _ (ChefTagRelation _ t)) -> t))
                           $ selectList [ChefTagRelationChefTagChef ==. chefId] []
                  newTags <- fmap catMaybes $ forM chefSettingsTags $ \t ->
                             liftIO (getChefTagId systemEnvDatabase t)
                  let toRemove = Set.fromList oldTags `Set.difference` Set.fromList newTags
                      toAdd = Set.fromList newTags `Set.difference` Set.fromList oldTags

                  forM_ toRemove $ \t ->
                    deleteWhere
                      [ ChefTagRelationChefTagChef ==. chefId
                      , ChefTagRelationChefTagChefTag ==. t
                      ]
                  forM_ toAdd $ \t ->
                    insert_ (ChefTagRelation chefId t)

                  pure (Just chefId)
