{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  , DeriveGeneric
  , OverloadedStrings
  #-}

module LocalCooking.Function.Farm where

import LocalCooking.Semantics.Farm ()
  -- ( FarmSettings (..), MenuSettings (..), MealSettings (..)
  -- )
import LocalCooking.Function.Semantics ()
  -- ( getFarmTags, getMealTags, getMenuTags, getMealIngredientsDiets
  -- , assignFarmTags, assignMealTags, assignMenuTags, assignMealIngredients
  -- )
import LocalCooking.Function.System (SystemM, SystemEnv (..), getUserId, guardRole, getSystemEnv)
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
import LocalCooking.Database.Query.IngredientDiet (getStoredIngredientId)

import Data.Maybe (catMaybes)
import Data.Aeson (ToJSON (..), FromJSON (..), (.=), object, (.:), Value (Object))
import Data.Aeson.Types (typeMismatch)
import GHC.Generics (Generic)
import Control.Monad (forM_, forM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.Persist (Entity (..), (==.), (=.))
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Class (selectList, getBy, insert, insert_, update, get)
