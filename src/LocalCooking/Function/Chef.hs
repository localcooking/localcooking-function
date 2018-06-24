{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  , DeriveGeneric
  , OverloadedStrings
  #-}

module LocalCooking.Function.Chef where

import LocalCooking.Function.Semantics
  ( getChefTags, getMealTags, getMenuTags, getMealIngredientsDiets
  , assignChefTags, assignMealTags, assignMenuTags, assignMealIngredients
  )
import LocalCooking.Function.System (SystemM, SystemEnv (..), getUserId, guardRole, getSystemEnv)
import LocalCooking.Semantics.Common (WithId (..))
import LocalCooking.Semantics.Chef
  ( GetSetChef (..), MenuSettings (..), MealSettings (..)
  )
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.User.Role (UserRole (Chef))
import LocalCooking.Database.Schema.Semantics
  ( StoredChef (..), StoredMenu (..), StoredMeal (..)
  , StoredChefId, StoredMenuId, StoredMealId
  , MenuTagRelation (..), ChefTagRelation (..), MealTagRelation (..)
  , MealIngredient (..)
  , EntityField
    ( StoredChefStoredChefName, StoredChefStoredChefPermalink
    , StoredChefStoredChefImages, StoredChefStoredChefAvatar
    , StoredChefStoredChefBio
    , StoredMenuStoredMenuAuthor, StoredMenuStoredMenuPublished
    , StoredMenuStoredMenuDeadline, StoredMenuStoredMenuDescription
    , StoredMenuStoredMenuHeading, StoredMenuStoredMenuImages
    , StoredMealStoredMealMenu, StoredMealStoredMealDescription
    , StoredMealStoredMealHeading, StoredMealStoredMealImages
    , StoredMealStoredMealInstructions, StoredMealStoredMealPermalink
    , StoredMealStoredMealPrice, StoredMealStoredMealTitle
    )
  , Unique
    ( UniqueChefOwner, UniqueMealPermalink, UniqueMenuDeadline)
  )
import LocalCooking.Database.Query.Semantics.Admin (hasRole)
import LocalCooking.Database.Query.Tag.Meal (insertMealTag, getMealTagId)
import LocalCooking.Database.Query.Tag.Chef (insertChefTag, getChefTagId)
import LocalCooking.Database.Query.IngredientDiet (getStoredIngredientTagId)

import Data.Maybe (catMaybes)
import Data.Aeson (ToJSON (..), FromJSON (..), (.=), object, (.:), Value (Object))
import Data.Aeson.Types (typeMismatch)
import GHC.Generics (Generic)
import Control.Monad (forM_, forM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.Persist (Entity (..), (==.), (=.))
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Class (selectList, getBy, insert, insert_, update, get)





getChef :: AuthToken -> SystemM (Maybe GetSetChef)
getChef authToken = do
  mChef <- getChefFromAuthToken authToken
  case mChef of
    Nothing -> pure Nothing
    Just (WithId chefId (StoredChef _ name permalink bio images avatar)) -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        mTags <- liftIO (getChefTags systemEnvDatabase chefId)
        case mTags of
          Nothing -> pure Nothing
          Just tags ->
            pure $ Just GetSetChef
              { getSetChefName = name
              , getSetChefPermalink = permalink
              , getSetChefImages = images
              , getSetChefAvatar = avatar
              , getSetChefBio = bio
              , getSetChefTags = tags
              }



setChef :: AuthToken -> GetSetChef -> SystemM (Maybe StoredChefId)
setChef authToken GetSetChef{..} = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure Nothing
    Just userId -> do
      mChef <- getChefFromAuthToken authToken
      case mChef of
        Nothing -> liftIO $ flip runSqlPool systemEnvDatabase $ do
          chefId <- insert $ StoredChef
            userId
            getSetChefName
            getSetChefPermalink
            getSetChefBio
            getSetChefImages
            getSetChefAvatar
          forM_ getSetChefTags $ \t -> do
            mChefTagId <- liftIO (getChefTagId systemEnvDatabase t)
            case mChefTagId of
              Nothing -> pure ()
              Just chefTagId -> insert_ (ChefTagRelation chefId chefTagId)
          pure (Just chefId)
        Just (WithId chefId _) -> do
          flip runSqlPool systemEnvDatabase $ update chefId
            [ StoredChefStoredChefName =. getSetChefName
            , StoredChefStoredChefPermalink =. getSetChefPermalink
            , StoredChefStoredChefBio =. getSetChefBio
            , StoredChefStoredChefImages =. getSetChefImages
            , StoredChefStoredChefAvatar =. getSetChefAvatar
            ]
          liftIO $ assignChefTags systemEnvDatabase chefId getSetChefTags
          pure (Just chefId)


getMenus :: AuthToken -> SystemM (Maybe [WithId StoredMenuId MenuSettings])
getMenus authToken = do
  mChef <- getChefFromAuthToken authToken
  case mChef of
    Nothing -> pure Nothing
    Just (WithId chefId _) -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        xs <- selectList [StoredMenuStoredMenuAuthor ==. chefId] []
        fmap (Just . catMaybes)
          $ forM xs $ \(Entity menuId (StoredMenu pub dead heading desc imgs _)) -> do
              mTags <- liftIO (getMenuTags systemEnvDatabase menuId)
              case mTags of
                Nothing -> pure Nothing
                Just tags ->
                  pure $ Just $ WithId
                    menuId
                    MenuSettings
                    { menuSettingsPublished = pub
                    , menuSettingsDeadline = dead
                    , menuSettingsHeading = heading
                    , menuSettingsDescription = desc
                    , menuSettingsImages = imgs
                    , menuSettingsTags = tags
                    }
                    


newMenu :: AuthToken -> MenuSettings -> SystemM (Maybe StoredMenuId)
newMenu authToken MenuSettings{..} = do
  mChef <- getChefFromAuthToken authToken
  case mChef of
    Nothing -> pure Nothing
    Just (WithId chefId _) -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        mEnt <- getBy (UniqueMenuDeadline chefId menuSettingsDeadline)
        case mEnt of
          Just _ -> pure Nothing
          Nothing -> do
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


setMenu :: AuthToken -> WithId StoredMenuId MenuSettings -> SystemM Bool
setMenu authToken (WithId menuId MenuSettings{..}) = do
  isAuthorized <- verifyChefhood authToken
  if not isAuthorized
    then pure False
    else do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      ok <- liftIO $ flip runSqlPool systemEnvDatabase $ do
        mEnt <- get menuId
        case mEnt of
          Nothing -> pure False
          _ -> do
            update menuId
              [ StoredMenuStoredMenuPublished =. menuSettingsPublished
              , StoredMenuStoredMenuDeadline =. menuSettingsDeadline
              , StoredMenuStoredMenuHeading =. menuSettingsHeading
              , StoredMenuStoredMenuDescription =. menuSettingsDescription
              , StoredMenuStoredMenuImages =. menuSettingsImages
              ]
            pure True
      if not ok
        then pure False
        else liftIO $ do
          assignMenuTags systemEnvDatabase menuId menuSettingsTags
          pure True


getMeals :: AuthToken -> StoredMenuId -> SystemM (Maybe [WithId StoredMealId MealSettings])
getMeals authToken menuId = do
  isAuthorized <- verifyChefhood authToken
  if not isAuthorized
    then pure Nothing
    else do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      xs <- liftIO $ flip runSqlPool systemEnvDatabase $
        selectList [StoredMealStoredMealMenu ==. menuId] []
      fmap (Just . catMaybes) $
        forM xs $ \(Entity mealId (StoredMeal title permalink _ heading desc inst imgs price)) -> do
          mTags <- liftIO (getMealTags systemEnvDatabase mealId)
          case mTags of
            Nothing -> pure Nothing
            Just tags -> do
              mIngDiets <- liftIO (getMealIngredientsDiets systemEnvDatabase mealId)
              case mIngDiets of
                Nothing -> pure Nothing
                Just (ings,_) ->
                  pure $ Just $ WithId
                    mealId
                    MealSettings
                    { mealSettingsTitle = title
                    , mealSettingsPermalink = permalink
                    , mealSettingsHeading = heading
                    , mealSettingsDescription = desc
                    , mealSettingsInstructions = inst
                    , mealSettingsImages = imgs
                    , mealSettingsIngredients = ings
                    , mealSettingsTags = tags
                    , mealSettingsPrice = price
                    }
                    


newMeal :: AuthToken -> WithId StoredMenuId MealSettings -> SystemM (Maybe StoredMealId)
newMeal authToken (WithId menuId MealSettings{..}) = do
  isAuthorized <- verifyChefhood authToken
  if not isAuthorized
    then pure Nothing
    else do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        mEnt <- getBy (UniqueMealPermalink menuId mealSettingsPermalink)
        case mEnt of
          Just _ -> pure Nothing
          Nothing -> do
            mealId <- insert $ StoredMeal
              mealSettingsTitle
              mealSettingsPermalink
              menuId
              mealSettingsHeading
              mealSettingsDescription
              mealSettingsInstructions
              mealSettingsImages
              mealSettingsPrice
            forM_ mealSettingsIngredients $ \ingName -> do
              mIngId <- liftIO (getStoredIngredientTagId systemEnvDatabase ingName)
              case mIngId of
                Nothing -> pure ()
                Just ingId -> insert_ (MealIngredient mealId ingId)
            forM_ mealSettingsTags $ \tag -> do
              mTagId <- liftIO (getMealTagId systemEnvDatabase tag)
              case mTagId of
                Nothing -> pure ()
                Just tagId -> insert_ (MealTagRelation mealId tagId)
            pure (Just mealId)


setMeal :: AuthToken -> WithId StoredMenuId (WithId StoredMealId MealSettings) -> SystemM Bool
setMeal authToken (WithId menuId (WithId mealId MealSettings{..})) = do
  isAuthorized <- verifyChefhood authToken
  if not isAuthorized
    then pure False
    else do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      ok <- liftIO $ flip runSqlPool systemEnvDatabase $ do
        mEnt <- get mealId
        case mEnt of
          Nothing -> pure False
          _ -> do
            update mealId
              [ StoredMealStoredMealTitle =. mealSettingsTitle
              , StoredMealStoredMealPermalink =. mealSettingsPermalink
              , StoredMealStoredMealMenu =. menuId
              , StoredMealStoredMealHeading =. mealSettingsHeading
              , StoredMealStoredMealDescription =. mealSettingsDescription
              , StoredMealStoredMealInstructions =. mealSettingsInstructions
              , StoredMealStoredMealImages =. mealSettingsImages
              , StoredMealStoredMealPrice =. mealSettingsPrice
              ]
            pure True
      if not ok
        then pure False
        else liftIO $ do
          assignMealTags systemEnvDatabase mealId mealSettingsTags
          assignMealIngredients systemEnvDatabase mealId mealSettingsIngredients
          pure True


verifyChefhood :: AuthToken -> SystemM Bool
verifyChefhood authToken = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure False
    Just userId -> guardRole userId Chef


getChefFromAuthToken :: AuthToken -> SystemM (Maybe (WithId StoredChefId StoredChef))
getChefFromAuthToken authToken = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure Nothing
    Just userId -> do
      isChef <- verifyChefhood authToken
      if not isChef
        then pure Nothing
        else do
          SystemEnv{systemEnvDatabase} <- getSystemEnv
          flip runSqlPool systemEnvDatabase $ do
            mEnt <- getBy (UniqueChefOwner userId)
            case mEnt of
              Nothing -> pure Nothing
              Just (Entity chefId chef) -> pure $ Just $ WithId chefId chef
