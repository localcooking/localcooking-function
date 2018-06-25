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
import LocalCooking.Function.System
  (SystemM, SystemEnv (..), getUserId, guardRole, getSystemEnv)
import LocalCooking.Semantics.Common (WithId (..))
import LocalCooking.Semantics.Chef
  ( GetSetChef (..), MenuSettings (..), MealSettings (..)
  )
import LocalCooking.Semantics.ContentRecord
  ( ContentRecord (ChefRecord), ChefRecord (..))
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.User.Role (UserRole (Chef))
import LocalCooking.Database.Schema
  ( StoredChef (..), StoredMenu (..), StoredMeal (..)
  , StoredChefId, StoredMenuId, StoredMealId, StoredUserId
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
    ( UniqueChefOwner, UniqueMealPermalink, UniqueMenuDeadline
    , UniqueStoredMealTag, UniqueStoredChefTag
    )
  , hasRole
  , getStoredIngredientTagId
  )
import LocalCooking.Database.Schema.Content
  ( StoredRecordSubmission (..)
  )

import Data.Maybe (catMaybes)
import Data.Aeson (ToJSON (..), FromJSON (..), (.=), object, (.:), Value (Object))
import Data.Aeson.Types (typeMismatch)
import Data.Time (getCurrentTime)
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
        mTags <- getChefTags chefId
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



unsafeSetChef :: StoredUserId -> GetSetChef -> SystemM (Maybe StoredChefId)
unsafeSetChef userId GetSetChef{..} = do
  mChef <- getChefFromUserId userId
  SystemEnv{systemEnvDatabase} <- getSystemEnv
  liftIO $ flip runSqlPool systemEnvDatabase $ case mChef of
    Nothing -> do
      chefId <- insert $ StoredChef
        userId
        getSetChefName
        getSetChefPermalink
        getSetChefBio
        getSetChefImages
        getSetChefAvatar
      forM_ getSetChefTags $ \t -> do
        mChefTagId <- getBy (UniqueStoredChefTag t)
        case mChefTagId of
          Nothing -> pure ()
          Just (Entity chefTagId _) -> insert_ (ChefTagRelation chefId chefTagId)
      pure (Just chefId)
    Just (WithId chefId _) -> do
      update chefId
        [ StoredChefStoredChefName =. getSetChefName
        , StoredChefStoredChefPermalink =. getSetChefPermalink
        , StoredChefStoredChefBio =. getSetChefBio
        , StoredChefStoredChefImages =. getSetChefImages
        , StoredChefStoredChefAvatar =. getSetChefAvatar
        ]
      assignChefTags chefId getSetChefTags
      pure (Just chefId)


setChef :: AuthToken -> GetSetChef -> SystemM Bool
setChef authToken getSetChef = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure False
    Just userId -> liftIO $ flip runSqlPool systemEnvDatabase $ do
      now <- liftIO getCurrentTime
      insert_ $ StoredRecordSubmission userId now $
        ChefRecord $ ChefRecordChef getSetChef
      pure True


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
              mTags <- getMenuTags menuId
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


unsafeNewMenu :: StoredUserId -> MenuSettings -> SystemM (Maybe StoredMenuId)
unsafeNewMenu userId MenuSettings{..} = do
  mChef <- getChefFromUserId userId
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
              mTagId <- getBy (UniqueStoredMealTag tag)
              case mTagId of
                Nothing -> pure ()
                Just (Entity tagId _) -> insert_ (MenuTagRelation menuId tagId)
            pure (Just menuId)


newMenu :: AuthToken -> MenuSettings -> SystemM Bool
newMenu authToken menu = do
  mChef <- getChefFromAuthToken authToken
  case mChef of
    Nothing -> pure False
    Just (WithId chefId _) -> do
      mUserId <- getUserId authToken
      case mUserId of
        Nothing -> pure False
        Just userId -> do
          SystemEnv{systemEnvDatabase} <- getSystemEnv
          liftIO $ flip runSqlPool systemEnvDatabase $ do
            now <- liftIO getCurrentTime
            insert_ $ StoredRecordSubmission userId now $
              ChefRecord $ ChefRecordNewMenu menu
            pure True


unsafeSetMenu :: StoredUserId -> WithId StoredMenuId MenuSettings -> SystemM Bool
unsafeSetMenu userId (WithId menuId MenuSettings{..}) = do
  isAuthorized <- guardRole userId Chef
  if not isAuthorized
    then pure False
    else do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
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
            assignMenuTags menuId menuSettingsTags
            pure True


setMenu :: AuthToken -> WithId StoredMenuId MenuSettings -> SystemM Bool
setMenu authToken menu = do
  isAuthorized <- verifyChefhood authToken
  if not isAuthorized
    then pure False
    else do
      mUserId <- getUserId authToken
      case mUserId of
        Nothing -> pure False
        Just userId -> do
          SystemEnv{systemEnvDatabase} <- getSystemEnv
          liftIO $ flip runSqlPool systemEnvDatabase $ do
            now <- liftIO getCurrentTime
            insert_ $ StoredRecordSubmission userId now $
              ChefRecord $ ChefRecordSetMenu menu
            pure True


getMeals :: AuthToken -> StoredMenuId -> SystemM (Maybe [WithId StoredMealId MealSettings])
getMeals authToken menuId = do
  isAuthorized <- verifyChefhood authToken
  if not isAuthorized
    then pure Nothing
    else do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        xs <- selectList [StoredMealStoredMealMenu ==. menuId] []
        fmap (Just . catMaybes) $
          forM xs $ \(Entity mealId (StoredMeal title permalink _ heading desc inst imgs price)) -> do
            mTags <- getMealTags mealId
            case mTags of
              Nothing -> pure Nothing
              Just tags -> do
                mIngDiets <- getMealIngredientsDiets mealId
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
              mIngId <- getStoredIngredientTagId ingName
              case mIngId of
                Nothing -> pure ()
                Just ingId -> insert_ (MealIngredient mealId ingId)
            forM_ mealSettingsTags $ \tag -> do
              mTagId <- getBy (UniqueStoredMealTag tag)
              case mTagId of
                Nothing -> pure ()
                Just (Entity tagId _) -> insert_ (MealTagRelation mealId tagId)
            pure (Just mealId)


setMeal :: AuthToken -> WithId StoredMenuId (WithId StoredMealId MealSettings) -> SystemM Bool
setMeal authToken (WithId menuId (WithId mealId MealSettings{..})) = do
  isAuthorized <- verifyChefhood authToken
  if not isAuthorized
    then pure False
    else do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
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
            assignMealTags mealId mealSettingsTags
            assignMealIngredients mealId mealSettingsIngredients
            pure True


verifyChefhood :: AuthToken -> SystemM Bool
verifyChefhood authToken = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure False
    Just userId -> guardRole userId Chef

getChefFromUserId :: StoredUserId -> SystemM (Maybe (WithId StoredChefId StoredChef))
getChefFromUserId userId = do
  isChef <- guardRole userId Chef
  if not isChef
    then pure Nothing
    else do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      flip runSqlPool systemEnvDatabase $ do
        mEnt <- getBy (UniqueChefOwner userId)
        case mEnt of
          Nothing -> pure Nothing
          Just (Entity chefId chef) -> pure $ Just $ WithId chefId chef

getChefFromAuthToken :: AuthToken -> SystemM (Maybe (WithId StoredChefId StoredChef))
getChefFromAuthToken authToken = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure Nothing
    Just userId -> getChefFromUserId userId
