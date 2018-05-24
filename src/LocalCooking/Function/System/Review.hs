{-# LANGUAGE
    RecordWildCards
  , NamedFieldPuns
  #-}

module LocalCooking.Function.System.Review where

import LocalCooking.Database.Schema.Semantics
  ( StoredChefId, StoredMealId, StoredReview (..)
  , EntityField (StoredReviewStoredReviewChef, StoredReviewStoredReviewMeal)
  )
import LocalCooking.Common.Rating (Rating, ratingToRational, ratingFromRational)
import LocalCooking.Semantics.Mitch (Review (..))

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Monad (forever, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, TVar, modifyTVar, newTVarIO, readTVar)
import Database.Persist (Entity (..), (==.))
import Database.Persist.Class (selectList)
import Database.Persist.Sql (ConnectionPool, runSqlPool)



data ReviewAccumulator = ReviewAccumulator
  { reviewAccumChefs :: TVar (HashMap StoredChefId (Rating, [Review]))
  , reviewAccumMeals :: TVar (HashMap StoredMealId Rating)
  }


newReviewAccumulator :: IO ReviewAccumulator
newReviewAccumulator = ReviewAccumulator <$> newTVarIO HashMap.empty <*> newTVarIO HashMap.empty


calculateThread :: ConnectionPool -> ReviewAccumulator -> IO ()
calculateThread db ReviewAccumulator{..} = forever $ do
  flip runSqlPool db $ do
    chefs <- selectList [] []
    forM_ chefs $ \(Entity chefId _) -> do
      reviews <- selectList [StoredReviewStoredReviewChef ==. chefId] []
      let totalRating =
            let go (Entity _ (StoredReview _ _ _ rating _ _ _ _ _)) acc =
                  (acc + ratingToRational rating) / 2
            in  ratingFromRational (foldr go 5 reviews)
          rs =
            let go (Entity i (StoredReview _ _ _ rating sub heading body images _)) = Review
                  { reviewRating = rating
                  , reviewSubmitted = sub
                  , reviewHeading = heading
                  , reviewBody = body
                  , reviewImages = images
                  , reviewId = i
                  }
            in  go <$> take 10 reviews

      liftIO $ atomically $ modifyTVar reviewAccumChefs $ HashMap.insert chefId (totalRating,rs)

    meals <- selectList [] []
    forM_ meals $ \(Entity mealId _) -> do
      reviews <- selectList [StoredReviewStoredReviewMeal ==. mealId] []
      let totalRating =
            let go (Entity _ (StoredReview _ _ _ rating _ _ _ _ _)) acc =
                  (acc + ratingToRational rating) / 2
            in  ratingFromRational (foldr go 5 reviews)

      liftIO $ atomically $ modifyTVar reviewAccumMeals $ HashMap.insert mealId totalRating

  threadDelay $
    let second = 10 ^ 6
        minute = second * 60
    in  10 * minute


lookupChefReviews :: ReviewAccumulator -> StoredChefId -> IO (Maybe (Rating, [Review]))
lookupChefReviews ReviewAccumulator{reviewAccumChefs} chefId = atomically $ do
  xs <- readTVar reviewAccumChefs
  pure (HashMap.lookup chefId xs)


lookupMealRating :: ReviewAccumulator -> StoredMealId -> IO (Maybe Rating)
lookupMealRating ReviewAccumulator{reviewAccumMeals} mealId = atomically $ do
  xs <- readTVar reviewAccumMeals
  pure (HashMap.lookup mealId xs)
