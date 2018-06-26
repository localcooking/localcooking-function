{-# LANGUAGE
    NamedFieldPuns
  , ScopedTypeVariables
  #-}

module LocalCooking.Function.Semantics where

import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Tag.Ingredient (IngredientTag)
import LocalCooking.Common.Tag.Diet (DietTag)
import LocalCooking.Database.Schema
  ( getDietId, getDiets, getDietById
  , getStoredIngredientTagId, getIngredientViolations
  , getIngredientTagById
  , StoredCustomerId, StoredChefId, StoredMealId, StoredMenuId
  , MenuTagRelation (..), ChefTagRelation (..), MealTagRelation (..), MealIngredient (..)
  , StoredAllergy (..), StoredDietPreference (..)
  , StoredMealTag (..), StoredChefTag (..)
  , StoredMealTagId, StoredChefTagId
  , EntityField
    ( StoredDietPreferenceOwner
    , StoredAllergyOwner
    , MenuTagRelationMenu
    , ChefTagRelationChef
    , MealTagRelationMeal
    , MealIngredientMeal
    )
  , Unique
    ( UniqueStoredMealTag, UniqueStoredChefTag, UniqueMealIngredient
    , UniqueMealTag, UniqueChefTag, UniqueMenuTag
    , UniqueDietPreference, UniqueAllergy
    )
  )

import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Control.Monad (forM, forM_)
import Control.Monad.Reader (ReaderT)
import Database.Persist (Entity (..), (==.))
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Class (selectList, get, getBy, insert_, deleteBy)



-- | Returns the set of non-violated diets, and used ingredients per diet
getMealIngredientsDiets :: StoredMealId -> ReaderT SqlBackend IO (Maybe ([IngredientTag],[DietTag]))
getMealIngredientsDiets mealId = do
  mMeal <- get mealId
  case mMeal of
    Nothing -> pure Nothing
    Just _ -> do
      xs <- selectList [MealIngredientMeal ==. mealId] []
      ( ings
        , ds :: [Set.Set DietTag]
        ) <- fmap (unzip . catMaybes) $ forM xs $ \(Entity _ (MealIngredient _ ingId)) -> do
        mIng <- getIngredientTagById ingId
        case mIng of
          Nothing -> pure Nothing
          Just ing -> do
            diets <- getIngredientViolations ingId
            pure $ Just (ing, Set.fromList diets)
      let violated = Set.unions ds
      allDiets <- Set.fromList <$> getDiets
      pure $ Just (ings, Set.toList (allDiets `Set.difference` violated))



getMealTags :: StoredMealId -> ReaderT SqlBackend IO (Maybe [MealTag])
getMealTags mealId = do
  mEnt <- get mealId
  case mEnt of
    Nothing -> pure Nothing
    Just _ -> do
      xs <- selectList [MealTagRelationMeal ==. mealId] []
      fmap (Just . catMaybes) $ forM xs $ \(Entity _ (MealTagRelation _ tagId)) -> do
        mStoredTag <- get tagId
        case mStoredTag of
          Nothing -> pure Nothing
          Just (StoredMealTag t) -> pure (Just t)


getMenuTags :: StoredMenuId -> ReaderT SqlBackend IO (Maybe [MealTag])
getMenuTags menuId = do
  mEnt <- get menuId
  case mEnt of
    Nothing -> pure Nothing
    Just _ -> do
      xs <- selectList [MenuTagRelationMenu ==. menuId] []
      fmap (Just . catMaybes) $ forM xs $ \(Entity _ (MenuTagRelation _ tagId)) -> do
        mStoredTag <- get tagId
        case mStoredTag of
          Nothing -> pure Nothing
          Just (StoredMealTag t) -> pure (Just t)


getChefTags :: StoredChefId -> ReaderT SqlBackend IO (Maybe [ChefTag])
getChefTags chefId = do
  mEnt <- get chefId
  case mEnt of
    Nothing -> pure Nothing
    Just _ -> do
      xs <- selectList [ChefTagRelationChef ==. chefId] []
      fmap (Just . catMaybes) $ forM xs $ \(Entity _ (ChefTagRelation _ tagId)) -> do
        mStoredTag <- get tagId
        case mStoredTag of
          Nothing -> pure Nothing
          Just (StoredChefTag t) -> pure (Just t)



assignChefTags :: StoredChefId -> [ChefTag] -> ReaderT SqlBackend IO ()
assignChefTags chefId chefSettingsTags = do
  oldTags <- fmap (fmap (\(Entity _ (ChefTagRelation _ t)) -> t))
            $ selectList [ChefTagRelationChef ==. chefId] []
  newTags <- fmap catMaybes $ forM chefSettingsTags $ \t -> do
    mStoredTag <- getBy (UniqueStoredChefTag t)
    case mStoredTag of
      Nothing -> pure Nothing
      Just (Entity j _) -> pure (Just j)

  let toRemove, toAdd :: Set.Set StoredChefTagId
      toRemove = Set.fromList oldTags `Set.difference` Set.fromList newTags
      toAdd = Set.fromList newTags `Set.difference` Set.fromList oldTags

  forM_ toRemove $ \t ->
    deleteBy (UniqueChefTag chefId t)
  forM_ toAdd $ \t ->
    insert_ (ChefTagRelation chefId t)



assignMenuTags :: StoredMenuId -> [MealTag] -> ReaderT SqlBackend IO ()
assignMenuTags menuId menuSettingsTags = do
  oldTags <- fmap (fmap (\(Entity _ (MenuTagRelation _ t)) -> t))
            $ selectList [MenuTagRelationMenu ==. menuId] []
  newTags <- fmap catMaybes $ forM menuSettingsTags $ \t -> do
    mStoredTag <- getBy (UniqueStoredMealTag t)
    case mStoredTag of
      Nothing -> pure Nothing
      Just (Entity j _) -> pure (Just j)

  let toRemove, toAdd :: Set.Set StoredMealTagId
      toRemove = Set.fromList oldTags `Set.difference` Set.fromList newTags
      toAdd = Set.fromList newTags `Set.difference` Set.fromList oldTags

  forM_ toRemove $ \t ->
    deleteBy (UniqueMenuTag menuId t)
  forM_ toAdd $ \t ->
    insert_ (MenuTagRelation menuId t)


assignMealTags :: StoredMealId -> [MealTag] -> ReaderT SqlBackend IO ()
assignMealTags mealId mealSettingsTags = do
  oldTags <- fmap (fmap (\(Entity _ (MealTagRelation _ t)) -> t))
            $ selectList [MealTagRelationMeal ==. mealId] []
  newTags <- fmap catMaybes $ forM mealSettingsTags $ \t -> do
    mStoredTag <- getBy (UniqueStoredMealTag t)
    case mStoredTag of
      Nothing -> pure Nothing
      Just (Entity j _) -> pure (Just j)

  let toRemove, toAdd :: Set.Set StoredMealTagId
      toRemove = Set.fromList oldTags `Set.difference` Set.fromList newTags
      toAdd = Set.fromList newTags `Set.difference` Set.fromList oldTags

  forM_ toRemove $ \t ->
    deleteBy (UniqueMealTag mealId t)
  forM_ toAdd $ \t ->
    insert_ (MealTagRelation mealId t)



assignMealIngredients :: StoredMealId -> [IngredientTag] -> ReaderT SqlBackend IO ()
assignMealIngredients mealId mealSettingsIngredients = do
  oldIngs <- fmap (fmap (\(Entity _ (MealIngredient _ t)) -> t))
            $ selectList [MealIngredientMeal ==. mealId] []
  newIngs <- fmap catMaybes $ forM mealSettingsIngredients $ \t ->
              getStoredIngredientTagId t

  let toRemove = Set.fromList oldIngs `Set.difference` Set.fromList newIngs
      toAdd = Set.fromList newIngs `Set.difference` Set.fromList oldIngs

  forM_ toRemove $ \t ->
    deleteBy (UniqueMealIngredient mealId t)
  forM_ toAdd $ \t ->
    insert_ (MealIngredient mealId t)


assignDiets :: StoredCustomerId -> [DietTag] -> ReaderT SqlBackend IO ()
assignDiets custId customerDiets = do
  oldDiets <- fmap (fmap (\(Entity _ (StoredDietPreference _ d)) -> d))
            $ selectList [StoredDietPreferenceOwner ==. custId] []
  newDiets <- fmap catMaybes $ forM customerDiets $ \d ->
              getDietId d
  let toRemove = Set.fromList oldDiets `Set.difference` Set.fromList newDiets
      toAdd = Set.fromList newDiets `Set.difference` Set.fromList oldDiets
  forM_ toRemove $ \d ->
    deleteBy (UniqueDietPreference custId d)
  forM_ toAdd $ \d ->
    insert_ (StoredDietPreference custId d)


getCustDiets :: StoredCustomerId -> ReaderT SqlBackend IO (Maybe [DietTag])
getCustDiets custId = do
  mCust <- get custId
  case mCust of
    Nothing -> pure Nothing
    Just _ -> do
      ds <- fmap (fmap (\(Entity _ (StoredDietPreference _ d)) -> d))
          $ selectList [StoredDietPreferenceOwner ==. custId] []
      fmap (Just . catMaybes) $ forM ds getDietById


assignAllergies :: StoredCustomerId -> [IngredientTag] -> ReaderT SqlBackend IO ()
assignAllergies custId customerAllergies = do
  oldAllergys <- fmap (fmap (\(Entity _ (StoredAllergy _ d)) -> d))
                $ selectList [StoredAllergyOwner ==. custId] []
  newAllergys <- fmap catMaybes $ forM customerAllergies $ \i ->
                  getStoredIngredientTagId i
  let toRemove = Set.fromList oldAllergys `Set.difference` Set.fromList newAllergys
      toAdd = Set.fromList newAllergys `Set.difference` Set.fromList oldAllergys
  forM_ toRemove $ \i -> deleteBy (UniqueAllergy custId i)
  forM_ toAdd $ \i -> insert_ (StoredAllergy custId i)


getCustAllergies :: StoredCustomerId -> ReaderT SqlBackend IO (Maybe [IngredientTag])
getCustAllergies custId = do
  mCust <- get custId
  case mCust of
    Nothing -> pure Nothing
    Just _ -> do
      ds <- fmap (fmap (\(Entity _ (StoredAllergy _ a)) -> a))
          $ selectList [StoredAllergyOwner ==. custId] []
      fmap (Just . catMaybes) $ forM ds getIngredientTagById
