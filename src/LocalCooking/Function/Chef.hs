{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  #-}

module LocalCooking.Function.Chef where

import LocalCooking.Semantics.Chef
  ( ChefSettings (..), MenuSettings (..)
  )
import LocalCooking.Function.System (AppM, SystemEnv (..), TokenContexts (..))
import LocalCooking.Function.System.AccessToken (lookupAccess)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.User.Role (UserRole (Chef))
import LocalCooking.Database.Schema.Semantics
  ( StoredChef (..), StoredMenu (..)
  , StoredChefId, StoredMenuId, StoredMealId
  , MenuTagRelation (..), ChefTagRelation (..), MealTagRelation (..)
  , EntityField
    ( StoredChefStoredChefName, StoredChefStoredChefPermalink
    , StoredChefStoredChefImages, StoredChefStoredChefAvatar
    , StoredChefStoredChefBio
    , StoredMenuStoredMenuAuthor, StoredMenuStoredMenuPublished
    , StoredMenuStoredMenuDeadline, StoredMenuStoredMenuDescription
    , StoredMenuStoredMenuHeading, StoredMenuStoredMenuImages
    , ChefTagRelationChefTagChef, ChefTagRelationChefTagChefTag
    , MenuTagRelationMenuTagMenu
    , MealTagRelationMealTagMeal
    )
  , Unique (UniqueChefOwner, UniqueChefTag, UniqueMealTag, UniqueMenuTag))
import LocalCooking.Database.Query.Semantics.Admin (hasRole)
import LocalCooking.Database.Query.Tag.Meal (insertMealTag, getMealTagId, getMealTagById)
import LocalCooking.Database.Query.Tag.Chef (insertChefTag, getChefTagId, getChefTagById)

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



assignChefTags :: StoredChefId -> [ChefTag] -> AppM ()
assignChefTags chefId chefSettingsTags = do
  SystemEnv{systemEnvDatabase} <- ask

  liftIO $ flip runSqlPool systemEnvDatabase $ do
    oldTags <- fmap (fmap (\(Entity _ (ChefTagRelation _ t)) -> t))
              $ selectList [ChefTagRelationChefTagChef ==. chefId] []
    newTags <- fmap catMaybes $ forM chefSettingsTags $ \t ->
                liftIO (getChefTagId systemEnvDatabase t)

    let toRemove = Set.fromList oldTags `Set.difference` Set.fromList newTags
        toAdd = Set.fromList newTags `Set.difference` Set.fromList oldTags

    forM_ toRemove $ \t ->
      deleteBy (UniqueChefTag chefId t)
    forM_ toAdd $ \t ->
      insert_ (ChefTagRelation chefId t)


assignMenuTags :: StoredMenuId -> [MealTag] -> AppM ()
assignMenuTags menuId menuSettingsTags = do
  SystemEnv{systemEnvDatabase} <- ask

  liftIO $ flip runSqlPool systemEnvDatabase $ do
    oldTags <- fmap (fmap (\(Entity _ (MenuTagRelation _ t)) -> t))
              $ selectList [MenuTagRelationMenuTagMenu ==. menuId] []
    newTags <- fmap catMaybes $ forM menuSettingsTags $ \t ->
                liftIO (getMealTagId systemEnvDatabase t)

    let toRemove = Set.fromList oldTags `Set.difference` Set.fromList newTags
        toAdd = Set.fromList newTags `Set.difference` Set.fromList oldTags

    forM_ toRemove $ \t ->
      deleteBy (UniqueMenuTag menuId t)
    forM_ toAdd $ \t ->
      insert_ (MenuTagRelation menuId t)


assignMealTags :: StoredMealId -> [MealTag] -> AppM ()
assignMealTags mealId mealSettingsTags = do
  SystemEnv{systemEnvDatabase} <- ask

  liftIO $ flip runSqlPool systemEnvDatabase $ do
    oldTags <- fmap (fmap (\(Entity _ (MealTagRelation _ t)) -> t))
              $ selectList [MealTagRelationMealTagMeal ==. mealId] []
    newTags <- fmap catMaybes $ forM mealSettingsTags $ \t ->
                liftIO (getMealTagId systemEnvDatabase t)

    let toRemove = Set.fromList oldTags `Set.difference` Set.fromList newTags
        toAdd = Set.fromList newTags `Set.difference` Set.fromList oldTags

    forM_ toRemove $ \t ->
      deleteBy (UniqueMealTag mealId t)
    forM_ toAdd $ \t ->
      insert_ (MealTagRelation mealId t)


getChef :: AuthToken -> AppM (Maybe ChefSettings)
getChef authToken = do
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
                Nothing -> pure Nothing
                Just (Entity chefId (StoredChef _ name permalink bio images avatar)) -> do
                  tagEnts <- selectList [ChefTagRelationChefTagChef ==. chefId] []
                  tags <- fmap catMaybes $ forM tagEnts $ \(Entity _ (ChefTagRelation _ tagId)) ->
                    liftIO (getChefTagById systemEnvDatabase tagId)
                  pure $ Just ChefSettings
                    { chefSettingsName = name
                    , chefSettingsPermalink = permalink
                    , chefSettingsImages = images
                    , chefSettingsAvatar = avatar
                    , chefSettingsBio = bio
                    , chefSettingsTags = tags
                    }


-- TODO FIXME separate validation routines for each stateful field?



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
            else do
              mChefId <- flip runSqlPool systemEnvDatabase $ do
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
                    pure $ Just chefId

              case mChefId of
                Nothing -> pure Nothing
                Just chefId -> do
                  assignChefTags chefId chefSettingsTags

                  pure (Just chefId)


getMenus :: AuthToken -> AppM (Maybe ([(StoredMenuId, MenuSettings)]))
getMenus authToken = do
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
                Nothing -> pure Nothing
                Just (Entity chefId _) -> do
                  xs <- selectList [StoredMenuStoredMenuAuthor ==. chefId] []
                  fmap Just
                    $ forM xs $ \(Entity menuId (StoredMenu pub dead head desc imgs _)) -> do
                        tags <- do
                          xs <- selectList [MenuTagRelationMenuTagMenu ==. menuId] []
                          fmap catMaybes $ forM xs $ \(Entity _ (MenuTagRelation _ tagId)) ->
                            liftIO (getMealTagById systemEnvDatabase tagId)
                        pure
                          ( menuId
                          , MenuSettings
                            { menuSettingsPublished = pub
                            , menuSettingsDeadline = dead
                            , menuSettingsHeading = head
                            , menuSettingsDescription = desc
                            , menuSettingsImages = imgs
                            , menuSettingsTags = tags
                            }
                          )


newMenu :: AuthToken -> MenuSettings -> AppM (Maybe StoredMenuId)
newMenu authToken MenuSettings{..} = do
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
                Nothing -> pure Nothing
                Just (Entity chefId _) -> do
                  menuId <- insert $ StoredMenu
                    menuSettingsPublished
                    menuSettingsDeadline
                    menuSettingsHeading
                    menuSettingsDescription
                    menuSettingsImages
                    chefId
                  forM_ menuSettingsTags $ \tag -> do
                    mTagId <- liftIO (getMealTagId systemEnvDatabase tag)
                    case mTagId of
                      Nothing -> pure ()
                      Just tagId -> insert_ (MenuTagRelation menuId tagId)
                  pure (Just menuId)


setMenu :: AuthToken -> StoredMenuId -> MenuSettings -> AppM Bool
setMenu authToken menuId MenuSettings{..} = do
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
            else flip runSqlPool systemEnvDatabase $ do
              mChefEnt <- getBy (UniqueChefOwner k)
              case mChefEnt of
                Nothing -> pure False
                Just (Entity chefId _) -> do
                  menuId <- insert $ StoredMenu
                    menuSettingsPublished
                    menuSettingsDeadline
                    menuSettingsHeading
                    menuSettingsDescription
                    menuSettingsImages
                    chefId
                  update menuId
                    [ StoredMenuStoredMenuPublished =. menuSettingsPublished
                    , StoredMenuStoredMenuDeadline =. menuSettingsDeadline
                    , StoredMenuStoredMenuHeading =. menuSettingsHeading
                    , StoredMenuStoredMenuDescription =. menuSettingsDescription
                    , StoredMenuStoredMenuImages =. menuSettingsImages
                    ]
                  forM_ menuSettingsTags $ \tag -> do
                    mTagId <- liftIO (getMealTagId systemEnvDatabase tag)
                    case mTagId of
                      Nothing -> pure ()
                      Just tagId -> insert_ (MenuTagRelation menuId tagId)
                  pure True


-- getMeals :: AuthToken -> StoredMenuId -> AppM [(StoredMealId, MealSettings)]
-- getMeals


-- setMeal :: AuthToken -> StoredMenuId -> MealSettings -> AppM (Maybe StoredMealId)
