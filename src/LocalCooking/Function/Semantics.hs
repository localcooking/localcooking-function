{-# LANGUAGE
    NamedFieldPuns
  , ScopedTypeVariables
  #-}

module LocalCooking.Function.Semantics where

import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Tag.Ingredient (IngredientTag)
import LocalCooking.Common.Tag.Diet (DietTag)
import LocalCooking.Database.Query.IngredientDiet
  ( getDietId, getDiets, getDietById
  , getStoredIngredientTagId, getIngredientViolations
  , getIngredientTagById)
import LocalCooking.Database.Query.Tag.Chef (getChefTagById, getChefTagId)
import LocalCooking.Database.Query.Tag.Meal (getMealTagById, getMealTagId)
import LocalCooking.Database.Schema.User.Customer
  ( StoredDietPreference (..)
  , StoredAllergy (..)
  , StoredCustomerId
  , EntityField
    ( StoredDietPreferenceDietPreferenceOwner
    , StoredAllergyAllergy, StoredAllergyAllergyOwner
    )
  , Unique (UniqueDietPreference))
import LocalCooking.Database.Schema.Semantics
  ( StoredChefId, StoredMealId, StoredMenuId
  , MenuTagRelation (..), ChefTagRelation (..), MealTagRelation (..), MealIngredient (..)
  , EntityField
    ( MenuTagRelationMenuTagMenu
    , ChefTagRelationChefTagChef
    , MealTagRelationMealTagMeal
    , MealIngredientMealIngredientMeal
    )
  , Unique
    ( UniqueMealTag, UniqueChefTag, UniqueMenuTag, UniqueMealIngredient
    )
  )

import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.Persist (Entity (..), (==.))
import Database.Persist.Sql (runSqlPool, ConnectionPool)
import Database.Persist.Class (selectList, get, insert_, deleteBy, deleteWhere)



-- | Returns the set of non-violated diets, and used ingredients per diet
getMealIngredientsDiets :: ConnectionPool -> StoredMealId -> IO (Maybe ([IngredientTag],[DietTag]))
getMealIngredientsDiets systemEnvDatabase mealId =
  flip runSqlPool systemEnvDatabase $ do
    mMeal <- get mealId
    case mMeal of
      Nothing -> pure Nothing
      Just _ -> do
        xs <- selectList [MealIngredientMealIngredientMeal ==. mealId] []
        ( ings
          , ds :: [Set.Set DietTag]
          ) <- fmap (unzip . catMaybes) $ forM xs $ \(Entity _ (MealIngredient _ ingId)) -> liftIO $ do
          mIng <- liftIO (getIngredientTagById systemEnvDatabase ingId)
          case mIng of
            Nothing -> pure Nothing
            Just ing -> do
              diets <- liftIO (getIngredientViolations systemEnvDatabase ingId)
              pure $ Just (ing,Set.fromList diets)
        let violated = Set.unions ds
        allDiets <- Set.fromList <$> liftIO (getDiets systemEnvDatabase)
        pure $ Just (ings, Set.toList (allDiets `Set.difference` violated))



getMealTags :: ConnectionPool -> StoredMealId -> IO (Maybe [MealTag])
getMealTags systemEnvDatabase mealId =
  flip runSqlPool systemEnvDatabase $ do
    mEnt <- get mealId
    case mEnt of
      Nothing -> pure Nothing
      Just _ -> do
        xs <- selectList [MealTagRelationMealTagMeal ==. mealId] []
        fmap (Just . catMaybes) $ forM xs $ \(Entity _ (MealTagRelation _ tagId)) ->
          liftIO (getMealTagById systemEnvDatabase tagId)


getMenuTags :: ConnectionPool -> StoredMenuId -> IO (Maybe [MealTag])
getMenuTags systemEnvDatabase menuId =
  flip runSqlPool systemEnvDatabase $ do
    mEnt <- get menuId
    case mEnt of
      Nothing -> pure Nothing
      Just _ -> do
        xs <- selectList [MenuTagRelationMenuTagMenu ==. menuId] []
        fmap (Just . catMaybes) $ forM xs $ \(Entity _ (MenuTagRelation _ tagId)) ->
          liftIO (getMealTagById systemEnvDatabase tagId)


getChefTags :: ConnectionPool -> StoredChefId -> IO (Maybe [ChefTag])
getChefTags systemEnvDatabase chefId =
  flip runSqlPool systemEnvDatabase $ do
    mEnt <- get chefId
    case mEnt of
      Nothing -> pure Nothing
      Just _ -> do
        xs <- selectList [ChefTagRelationChefTagChef ==. chefId] []
        fmap (Just . catMaybes) $ forM xs $ \(Entity _ (ChefTagRelation _ tagId)) ->
          liftIO (getChefTagById systemEnvDatabase tagId)



assignChefTags :: ConnectionPool -> StoredChefId -> [ChefTag] -> IO ()
assignChefTags systemEnvDatabase chefId chefSettingsTags =
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
assignMenuTags systemEnvDatabase menuId menuSettingsTags =
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
assignMealTags systemEnvDatabase mealId mealSettingsTags =
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



assignMealIngredients :: ConnectionPool -> StoredMealId -> [IngredientTag] -> IO ()
assignMealIngredients systemEnvDatabase mealId mealSettingsIngredients =
  flip runSqlPool systemEnvDatabase $ do
    oldIngs <- fmap (fmap (\(Entity _ (MealIngredient _ t)) -> t))
              $ selectList [MealIngredientMealIngredientMeal ==. mealId] []
    newIngs <- fmap catMaybes $ forM mealSettingsIngredients $ \t ->
                liftIO (getStoredIngredientTagId systemEnvDatabase t)

    let toRemove = Set.fromList oldIngs `Set.difference` Set.fromList newIngs
        toAdd = Set.fromList newIngs `Set.difference` Set.fromList oldIngs

    forM_ toRemove $ \t ->
      deleteBy (UniqueMealIngredient mealId t)
    forM_ toAdd $ \t ->
      insert_ (MealIngredient mealId t)


assignDiets :: ConnectionPool -> StoredCustomerId -> [DietTag] -> IO ()
assignDiets systemEnvDatabase custId customerDiets =
  flip runSqlPool systemEnvDatabase $ do
    oldDiets <- fmap (fmap (\(Entity _ (StoredDietPreference _ d)) -> d))
              $ selectList [StoredDietPreferenceDietPreferenceOwner ==. custId] []
    newDiets <- fmap catMaybes $ forM customerDiets $ \d ->
                liftIO (getDietId systemEnvDatabase d)
    let toRemove = Set.fromList oldDiets `Set.difference` Set.fromList newDiets
        toAdd = Set.fromList newDiets `Set.difference` Set.fromList oldDiets
    forM_ toRemove $ \d ->
      deleteBy (UniqueDietPreference custId d)
    forM_ toAdd $ \d ->
      insert_ (StoredDietPreference custId d)


getCustDiets :: ConnectionPool -> StoredCustomerId -> IO (Maybe [DietTag])
getCustDiets systemEnvDatabase custId =
  flip runSqlPool systemEnvDatabase $ do
    mCust <- get custId
    case mCust of
      Nothing -> pure Nothing
      Just _ -> do
        ds <- fmap (fmap (\(Entity _ (StoredDietPreference _ d)) -> d))
            $ selectList [StoredDietPreferenceDietPreferenceOwner ==. custId] []
        fmap (Just . catMaybes) $ forM ds $ liftIO . getDietById systemEnvDatabase



assignAllergies :: ConnectionPool -> StoredCustomerId -> [IngredientTag] -> IO ()
assignAllergies systemEnvDatabase custId customerAllergies =
  flip runSqlPool systemEnvDatabase $ do
    oldAllergys <- fmap (fmap (\(Entity _ (StoredAllergy _ d)) -> d))
                $ selectList [StoredAllergyAllergyOwner ==. custId] []
    newAllergys <- fmap catMaybes $ forM customerAllergies $ \i ->
                  liftIO (getStoredIngredientTagId systemEnvDatabase i)
    let toRemove = Set.fromList oldAllergys `Set.difference` Set.fromList newAllergys
        toAdd = Set.fromList newAllergys `Set.difference` Set.fromList oldAllergys
    forM_ toRemove $ \i -> deleteWhere
      [ StoredAllergyAllergyOwner ==. custId
      , StoredAllergyAllergy ==. i
      ]
    forM_ toAdd $ \i -> insert_ (StoredAllergy custId i)


getCustAllergies :: ConnectionPool -> StoredCustomerId -> IO (Maybe [IngredientTag])
getCustAllergies systemEnvDatabase custId =
  flip runSqlPool systemEnvDatabase $ do
    mCust <- get custId
    case mCust of
      Nothing -> pure Nothing
      Just _ -> do
        ds <- fmap (fmap (\(Entity _ (StoredAllergy _ a)) -> a))
            $ selectList [StoredAllergyAllergyOwner ==. custId] []
        fmap (Just . catMaybes) $ forM ds $ liftIO . getIngredientTagById systemEnvDatabase
