{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  , ScopedTypeVariables
  #-}

module LocalCooking.Function.Semantics where

import LocalCooking.Semantics.Mitch
  ( Customer (..)
  , Review (..)
  , Chef (..), ChefSynopsis (..)
  , MenuSynopsis (..), Menu (..)
  , MealSynopsis (..), Meal (..)
  , Order (..)
  , getReviewSynopsis
  )
import LocalCooking.Function.System (AppM, SystemEnv (..), TokenContexts (..), getUserId)
import LocalCooking.Function.System.Review (lookupChefReviews, lookupMealRating)
import LocalCooking.Function.System.AccessToken (lookupAccess)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Order (OrderProgress (DeliveredProgress))
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Ingredient (IngredientName)
import LocalCooking.Common.Diet (Diet)
import LocalCooking.Database.Query.IngredientDiet
  ( getDietId, getDiets
  , getStoredIngredientId, getIngredientViolations
  , getIngredientById, getIngredientNameById, getIngredientByName)
import LocalCooking.Database.Query.Tag.Chef (getChefTagById, getChefTagId)
import LocalCooking.Database.Query.Tag.Meal (getMealTagById, getMealTagId)
import LocalCooking.Database.Query.Semantics (getMealId)
import LocalCooking.Database.Schema.User.Customer
  ( StoredDietPreference (..)
  , StoredCustomer (..), StoredAllergy (..)
  , StoredCustomerId
  , EntityField
    ( StoredDietPreferenceDietPreferenceOwner
    , StoredDietPreferenceDietPreferenceDiet, StoredCustomerStoredCustomerAddress
    , StoredCustomerStoredCustomerName
    , StoredAllergyAllergy, StoredAllergyAllergyOwner
    )
  , Unique (UniqueCustomer, UniqueDietPreference))
import LocalCooking.Database.Schema.Semantics
  ( StoredMenu (..), StoredChef (..), StoredOrder (..), StoredMeal (..), StoredReview (..)
  , StoredChefId, StoredMealId, StoredMenuId, StoredReviewId
  , MenuTagRelation (..), ChefTagRelation (..), MealTagRelation (..), MealIngredient (..)
  , CartRelation (..)
  , EntityField
    ( MenuTagRelationMenuTagMenu, StoredMenuStoredMenuAuthor
    , StoredOrderStoredOrderChef, StoredMenuStoredMenuDeadline
    , StoredOrderStoredOrderMenu, ChefTagRelationChefTagChef
    , StoredOrderStoredOrderCustomer, MealTagRelationMealTagMeal
    , StoredOrderStoredOrderProgress, StoredOrderStoredOrderMeal
    , StoredMealStoredMealMenu, StoredReviewStoredReviewMeal
    , MealIngredientMealIngredientMeal
    , CartRelationCartRelationAdded, CartRelationCartRelationVolume
    , CartRelationCartRelationCustomer
    )
  , Unique
    ( UniqueChefPermalink, UniqueMealPermalink, UniqueMenuDeadline, UniqueCartRelation
    , UniqueMealTag, UniqueChefTag, UniqueMenuTag, UniqueMealIngredient
    )
  )

import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Data.Text.Permalink (Permalink)
import Data.IORef (newIORef, readIORef, modifyIORef)
import Data.Time (UTCTime, getCurrentTime, utctDay)
import Data.Time.Calendar (Day)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import Database.Persist (Entity (..), (==.), (=.), (>=.), (!=.))
import Database.Persist.Sql (runSqlPool, ConnectionPool)
import Database.Persist.Class (selectList, get, getBy, insert, insert_, deleteBy, deleteWhere, update, count)



-- | Returns the set of non-violated diets, and used ingredients per diet
getMealIngredientsDiets :: ConnectionPool -> StoredMealId -> IO (Maybe ([IngredientName],[Diet]))
getMealIngredientsDiets systemEnvDatabase mealId = do
  flip runSqlPool systemEnvDatabase $ do
    mMeal <- get mealId
    case mMeal of
      Nothing -> pure Nothing
      Just _ -> do
        xs <- selectList [MealIngredientMealIngredientMeal ==. mealId] []
        ( ings
          , ds :: [Set.Set Diet]
          ) <- fmap (unzip . catMaybes) $ forM xs $ \(Entity _ (MealIngredient _ ingId)) -> liftIO $ do
          mIng <- liftIO (getIngredientNameById systemEnvDatabase ingId)
          case mIng of
            Nothing -> pure Nothing
            Just ing -> do
              diets <- liftIO (getIngredientViolations systemEnvDatabase ingId)
              pure $ Just (ing,Set.fromList diets)
        let violated = Set.unions ds
        allDiets <- Set.fromList <$> liftIO (getDiets systemEnvDatabase)
        pure $ Just (ings, Set.toList (allDiets `Set.difference` violated))



getMealTags :: ConnectionPool -> StoredMealId -> IO (Maybe [MealTag])
getMealTags systemEnvDatabase mealId = do
  flip runSqlPool systemEnvDatabase $ do
    mEnt <- get mealId
    case mEnt of
      Nothing -> pure Nothing
      Just _ -> do
        xs <- selectList [MealTagRelationMealTagMeal ==. mealId] []
        fmap (Just . catMaybes) $ forM xs $ \(Entity _ (MealTagRelation _ tagId)) ->
          liftIO (getMealTagById systemEnvDatabase tagId)


getMenuTags :: ConnectionPool -> StoredMenuId -> IO (Maybe [MealTag])
getMenuTags systemEnvDatabase menuId = do
  flip runSqlPool systemEnvDatabase $ do
    mEnt <- get menuId
    case mEnt of
      Nothing -> pure Nothing
      Just _ -> do
        xs <- selectList [MenuTagRelationMenuTagMenu ==. menuId] []
        fmap (Just . catMaybes) $ forM xs $ \(Entity _ (MenuTagRelation _ tagId)) ->
          liftIO (getMealTagById systemEnvDatabase tagId)


getChefTags :: ConnectionPool -> StoredChefId -> IO (Maybe [ChefTag])
getChefTags systemEnvDatabase chefId = do
  flip runSqlPool systemEnvDatabase $ do
    mEnt <- get chefId
    case mEnt of
      Nothing -> pure Nothing
      Just _ -> do
        xs <- selectList [ChefTagRelationChefTagChef ==. chefId] []
        fmap (Just . catMaybes) $ forM xs $ \(Entity _ (ChefTagRelation _ tagId)) ->
          liftIO (getChefTagById systemEnvDatabase tagId)



assignChefTags :: ConnectionPool -> StoredChefId -> [ChefTag] -> IO ()
assignChefTags systemEnvDatabase chefId chefSettingsTags = do
  flip runSqlPool systemEnvDatabase $ do
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



assignMenuTags :: ConnectionPool -> StoredMenuId -> [MealTag] -> IO ()
assignMenuTags systemEnvDatabase menuId menuSettingsTags = do
  flip runSqlPool systemEnvDatabase $ do
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


assignMealTags :: ConnectionPool -> StoredMealId -> [MealTag] -> IO ()
assignMealTags systemEnvDatabase mealId mealSettingsTags = do
  flip runSqlPool systemEnvDatabase $ do
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



assignMealIngredients :: ConnectionPool -> StoredMealId -> [IngredientName] -> IO ()
assignMealIngredients systemEnvDatabase mealId mealSettingsIngredients = do
  flip runSqlPool systemEnvDatabase $ do
    oldIngs <- fmap (fmap (\(Entity _ (MealIngredient _ t)) -> t))
              $ selectList [MealIngredientMealIngredientMeal ==. mealId] []
    newIngs <- fmap catMaybes $ forM mealSettingsIngredients $ \t ->
                liftIO (getStoredIngredientId systemEnvDatabase t)

    let toRemove = Set.fromList oldIngs `Set.difference` Set.fromList newIngs
        toAdd = Set.fromList newIngs `Set.difference` Set.fromList oldIngs

    forM_ toRemove $ \t ->
      deleteBy (UniqueMealIngredient mealId t)
    forM_ toAdd $ \t ->
      insert_ (MealIngredient mealId t)
