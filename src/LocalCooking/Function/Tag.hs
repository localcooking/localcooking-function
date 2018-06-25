{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  , DeriveGeneric
  , OverloadedStrings
  , GADTs
  , FlexibleContexts
  #-}

module LocalCooking.Function.Tag where

import LocalCooking.Function.System (SystemM, SystemEnv (..), getUserId, guardRole, getSystemEnv)
import LocalCooking.Semantics.ContentRecord
  ( ContentRecord (TagRecord), contentRecordVariant
  , TagRecord (TagRecordChef, TagRecordMeal)
  )
import LocalCooking.Database.Schema
  ( StoredChefTag (..), StoredCultureTag (..)
  , StoredDietTag (..), StoredFarmTag (..)
  , StoredIngredientTag (..), StoredMealTag (..)
  , StoredUserId
  )
import LocalCooking.Database.Schema.Content
  ( StoredRecordSubmission (..)
  )
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
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Logging (log')
import Text.Search.Sphinx (query, defaultConfig)
import Text.Search.Sphinx.Types
  (MatchMode (Any), Result (Ok), QueryResult (matches), Match (documentId))
import Text.Search.Sphinx.Configuration (Configuration (mode, port, limit))
import Database.Persist (PersistEntity (PersistEntityBackend), ToBackendKey)
import Database.Persist.Sql (runSqlPool, toSqlKey, SqlBackend)
import Database.Persist.Class (selectList, get, getBy, insert, insert_, update, get)



-- * Unsafe Storage

unsafeStoreChefTag :: ChefTag -> SystemM ()
unsafeStoreChefTag tag = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv
  flip runSqlPool systemEnvDatabase $ do
    insert_ (StoredChefTag tag)

unsafeStoreCultureTag :: CultureTag -> SystemM ()
unsafeStoreCultureTag tag = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv
  flip runSqlPool systemEnvDatabase $ do
    insert_ (StoredCultureTag tag)

unsafeStoreDietTag :: DietTag -> SystemM ()
unsafeStoreDietTag tag = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv
  flip runSqlPool systemEnvDatabase $ do
    insert_ (StoredDietTag tag)

unsafeStoreFarmTag :: FarmTag -> SystemM ()
unsafeStoreFarmTag tag = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv
  flip runSqlPool systemEnvDatabase $ do
    insert_ (StoredFarmTag tag)

unsafeStoreIngredientTag :: IngredientTag -> SystemM ()
unsafeStoreIngredientTag tag = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv
  flip runSqlPool systemEnvDatabase $ do
    insert_ (StoredIngredientTag tag)

unsafeStoreMealTag :: MealTag -> SystemM ()
unsafeStoreMealTag tag = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv
  flip runSqlPool systemEnvDatabase $ do
    insert_ (StoredMealTag tag)


-- * Safe Storage

-- | As a content record submission
submitTag :: StoredUserId -> TagRecord -> SystemM ()
submitTag userId tag = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv
  flip runSqlPool systemEnvDatabase $ do
    now <- liftIO getCurrentTime
    let record = TagRecord tag
    insert_ $ StoredRecordSubmission userId now record (contentRecordVariant record)


submitChefTag :: StoredUserId -> ChefTag -> SystemM ()
submitChefTag userId = submitTag userId . TagRecordChef


submitMealTag :: StoredUserId -> MealTag -> SystemM ()
submitMealTag userId = submitTag userId . TagRecordMeal


-- * Search

searchChefTags :: Text -> SystemM (Maybe [ChefTag])
searchChefTags = searchGeneric (\(StoredChefTag x) -> x) "cheftags"


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
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      let ks = (toSqlKey . documentId) <$> matches xs
      flip runSqlPool systemEnvDatabase $
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
