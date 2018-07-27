{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  , DeriveGeneric
  , OverloadedStrings
  , GADTs
  , FlexibleContexts
  #-}

module LocalCooking.Function.Tag
  ( unsafeStoreChefTag, unsafeStoreCultureTag, unsafeStoreDietTag
  , unsafeStoreFarmTag, unsafeStoreIngredientTag, unsafeStoreMealTag
  , submitChefTag, submitCultureTag, submitDietTag
  , submitFarmTag, submitIngredientTag, submitMealTag
  , searchChefTags, searchCultureTags, searchDietTags
  , searchFarmTags, searchIngredientTags, searchMealTags
  ) where

import LocalCooking.Function.System
  (SystemM, SystemEnv (..), getSystemEnv, liftDb)
import LocalCooking.Function.User (getUserId)
import LocalCooking.Semantics.ContentRecord
  ( ContentRecord (TagRecord), contentRecordVariant
  , TagRecord (..)
  )
import LocalCooking.Semantics.User (UserExists (..))
import LocalCooking.Database.Schema
  ( StoredChefTag (..), StoredCultureTag (..)
  , StoredDietTag (..), StoredFarmTag (..)
  , StoredIngredientTag (..), StoredMealTag (..)
  )
import LocalCooking.Database.Schema.Content
  ( StoredRecordSubmission (..)
  )
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Tag.Culture (CultureTag)
import LocalCooking.Common.Tag.Diet (DietTag)
import LocalCooking.Common.Tag.Farm (FarmTag)
import LocalCooking.Common.Tag.Ingredient (IngredientTag)
import LocalCooking.Common.Tag.Meal (MealTag)

import Data.Time (getCurrentTime)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Data.Maybe (catMaybes)
import Data.Aeson.JSONUnit (JSONUnit (..))
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Logging (log')
import Text.Search.Sphinx (query, defaultConfig)
import Text.Search.Sphinx.Types
  (MatchMode (Any), Result (Ok), QueryResult (matches), Match (documentId))
import Text.Search.Sphinx.Configuration (Configuration (mode, port, limit))
import Database.Persist (PersistEntity (PersistEntityBackend), ToBackendKey)
import Database.Persist.Sql (runSqlPool, toSqlKey, SqlBackend)
import Database.Persist.Class (get, insert_, get)



-- * Unsafe Storage

unsafeStoreChefTag :: ChefTag -> SystemM ()
unsafeStoreChefTag tag = do
  liftDb $ insert_ (StoredChefTag tag)

unsafeStoreCultureTag :: CultureTag -> SystemM ()
unsafeStoreCultureTag tag = do
  liftDb $ insert_ (StoredCultureTag tag)

unsafeStoreDietTag :: DietTag -> SystemM ()
unsafeStoreDietTag tag = do
  liftDb $ insert_ (StoredDietTag tag)

unsafeStoreFarmTag :: FarmTag -> SystemM ()
unsafeStoreFarmTag tag = do
  liftDb $ insert_ (StoredFarmTag tag)

unsafeStoreIngredientTag :: IngredientTag -> SystemM ()
unsafeStoreIngredientTag tag = do
  liftDb $ insert_ (StoredIngredientTag tag)

unsafeStoreMealTag :: MealTag -> SystemM ()
unsafeStoreMealTag tag = do
  liftDb $ insert_ (StoredMealTag tag)


-- * Safe Storage

-- | As a content record submission
submitTag :: AuthToken -> TagRecord -> SystemM (UserExists JSONUnit)
submitTag token tag = do
  mUserId <- getUserId token
  case mUserId of
    UserDoesntExist -> pure UserDoesntExist
    UserExists userId -> fmap UserExists $ liftDb $ do
      now <- liftIO getCurrentTime
      let record = TagRecord tag
      insert_ $ StoredRecordSubmission userId now record (contentRecordVariant record)
      pure JSONUnit


submitChefTag :: AuthToken -> ChefTag -> SystemM (UserExists JSONUnit)
submitChefTag token = submitTag token . TagRecordChef

submitCultureTag :: AuthToken -> CultureTag -> SystemM (UserExists JSONUnit)
submitCultureTag token = submitTag token . TagRecordCulture

submitDietTag :: AuthToken -> DietTag -> SystemM (UserExists JSONUnit)
submitDietTag token = submitTag token . TagRecordDiet

submitFarmTag :: AuthToken -> FarmTag -> SystemM (UserExists JSONUnit)
submitFarmTag token = submitTag token . TagRecordFarm

submitIngredientTag :: AuthToken -> IngredientTag -> SystemM (UserExists JSONUnit)
submitIngredientTag token = submitTag token . TagRecordIngredient

submitMealTag :: AuthToken -> MealTag -> SystemM (UserExists JSONUnit)
submitMealTag token = submitTag token . TagRecordMeal


-- * Search

searchChefTags :: Text -> SystemM (Maybe [ChefTag])
searchChefTags = searchGeneric (\(StoredChefTag x) -> x) "cheftags"

searchCultureTags :: Text -> SystemM (Maybe [CultureTag])
searchCultureTags = searchGeneric (\(StoredCultureTag x) -> x) "culturetags"

searchDietTags :: Text -> SystemM (Maybe [DietTag])
searchDietTags = searchGeneric (\(StoredDietTag x) -> x) "diettags"

searchFarmTags :: Text -> SystemM (Maybe [FarmTag])
searchFarmTags = searchGeneric (\(StoredFarmTag x) -> x) "farmtags"

searchIngredientTags :: Text -> SystemM (Maybe [IngredientTag])
searchIngredientTags = searchGeneric (\(StoredIngredientTag x) -> x) "ingredienttags"

searchMealTags :: Text -> SystemM (Maybe [MealTag])
searchMealTags = searchGeneric (\(StoredMealTag x) -> x) "mealtags"


-- TODO register different sphinx indicies for each tag, mount different http responses

searchGeneric :: PersistEntityBackend b ~ SqlBackend
              => PersistEntity b
              => ToBackendKey SqlBackend b
              => (b -> a) -> Text -> Text -> SystemM (Maybe [a])
searchGeneric fromRecord index term = do
  xs' <- liftIO (query config index term)
  case xs' of
    Ok xs -> do
      let ks = (toSqlKey . documentId) <$> matches xs
      liftDb $
        fmap (Just . catMaybes) $ forM ks $ \k -> do
          mX <- get k
          pure (fromRecord <$> mX)
    e -> do
      log' $ "Sphinx error: " <> T.pack (show e)
      pure Nothing
  where
    config = defaultConfig
      { port = 9312
      , mode = Any
      , limit = 10
      }
