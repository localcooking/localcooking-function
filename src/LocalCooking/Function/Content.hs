{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  , DeriveGeneric
  , OverloadedStrings
  #-}

module LocalCooking.Function.Content where

import LocalCooking.Semantics.Content (EditorSettings (..))
import LocalCooking.Function.Semantics ()
import LocalCooking.Function.System (SystemM, SystemEnv (..), getUserId, guardRole, getSystemEnv)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.User.Role (UserRole (Chef))
import LocalCooking.Database.Schema.User.Editor
  ( StoredEditor (..), Unique (UniqueEditor))
-- import LocalCooking.Database.Schema.Semantics
--   ( StoredChef (..), StoredMenu (..), StoredMeal (..)
--   , StoredChefId, StoredMenuId, StoredMealId
--   , MenuTagRelation (..), ChefTagRelation (..), MealTagRelation (..)
--   , MealIngredient (..)
--   , EntityField
--     ( StoredChefStoredChefName, StoredChefStoredChefPermalink
--     , StoredChefStoredChefImages, StoredChefStoredChefAvatar
--     , StoredChefStoredChefBio
--     , StoredMenuStoredMenuAuthor, StoredMenuStoredMenuPublished
--     , StoredMenuStoredMenuDeadline, StoredMenuStoredMenuDescription
--     , StoredMenuStoredMenuHeading, StoredMenuStoredMenuImages
--     , StoredMealStoredMealMenu, StoredMealStoredMealDescription
--     , StoredMealStoredMealHeading, StoredMealStoredMealImages
--     , StoredMealStoredMealInstructions, StoredMealStoredMealPermalink
--     , StoredMealStoredMealPrice, StoredMealStoredMealTitle
--     )
--   , Unique
--     ( UniqueChefOwner, UniqueMealPermalink, UniqueMenuDeadline)
--   )
import LocalCooking.Database.Query.Semantics.Admin (hasRole)
import LocalCooking.Database.Query.Tag.Meal (insertMealTag, getMealTagId)
import LocalCooking.Database.Query.Tag.Chef (insertChefTag, getChefTagId)

import Data.Maybe (catMaybes)
import Data.Aeson (ToJSON (..), FromJSON (..), (.=), object, (.:), Value (Object))
import Data.Aeson.Types (typeMismatch)
import GHC.Generics (Generic)
import Control.Monad (forM_, forM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.Persist (Entity (..), (==.), (=.))
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Class (selectList, getBy, insert, insert_, update, get)




setEditor :: AuthToken -> EditorSettings -> SystemM Bool
setEditor authToken EditorSettings{..} = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure False
    Just userId -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        mCustEnt <- getBy (UniqueEditor userId)
        case mCustEnt of
          Nothing -> do
            insert_ (StoredEditor userId)
            pure True
          Just _ ->
            pure True
