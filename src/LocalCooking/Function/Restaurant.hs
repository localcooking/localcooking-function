{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  , DeriveGeneric
  , OverloadedStrings
  #-}

module LocalCooking.Function.Restaurant where

import LocalCooking.Semantics.Restaurant ()
import LocalCooking.Function.Semantics ()
import LocalCooking.Function.System (SystemM, SystemEnv (..), getUserId, guardRole, getSystemEnv)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.User.Role (UserRole (Chef))
import LocalCooking.Database.Schema
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

import Data.Maybe (catMaybes)
import Data.Aeson (ToJSON (..), FromJSON (..), (.=), object, (.:), Value (Object))
import Data.Aeson.Types (typeMismatch)
import GHC.Generics (Generic)
import Control.Monad (forM_, forM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.Persist (Entity (..), (==.), (=.))
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Class (selectList, getBy, insert, insert_, update, get)
