{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  , ScopedTypeVariables
  #-}

module LocalCooking.Function.Mitch where

import LocalCooking.Semantics.Mitch
  ( Customer (..)
  , Review (..)
  , Chef (..), ChefSynopsis (..)
  , MenuSynopsis (..), Menu (..)
  , MealSynopsis (..), Meal (..)
  , Order (..)
  , getReviewSynopsis
  )
import LocalCooking.Function.System (AppM, SystemEnv (..), TokenContexts (..))
import LocalCooking.Function.System.Review (lookupChefReviews, lookupMealRating)
import LocalCooking.Function.System.AccessToken (lookupAccess)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Order (OrderProgress (DeliveredProgress))
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Ingredient (IngredientName)
import LocalCooking.Common.Diet (Diet)
import LocalCooking.Database.Query.IngredientDiet
  ( getDietId, getDiets
  , getStoredIngredientId, getIngredientViolations
  , getIngredientById, getIngredientNameById, getIngredientByName)
import LocalCooking.Database.Query.Tag.Chef (getChefTagById)
import LocalCooking.Database.Query.Tag.Meal (getMealTagById)
import LocalCooking.Database.Schema.User.Customer
  ( StoredDietPreference (..)
  , StoredCustomer (..), StoredAllergy (..)
  , EntityField
    ( StoredDietPreferenceDietPreferenceOwner
    , StoredDietPreferenceDietPreferenceDiet, StoredCustomerStoredCustomerAddress
    , StoredCustomerStoredCustomerName
    , StoredAllergyAllergy, StoredAllergyAllergyOwner
    )
  , Unique (UniqueCustomer))
import LocalCooking.Database.Schema.Semantics
  ( StoredMenu (..), StoredChef (..), StoredOrder (..), StoredMeal (..), StoredReview (..)
  , StoredChefId, StoredMealId, StoredMenuId, StoredReviewId
  , MenuTagRelation (..), ChefTagRelation (..), MealTagRelation (..), MealIngredient (..)
  , EntityField
    ( MenuTagRelationMenuTagMenu, StoredMenuStoredMenuAuthor
    , StoredOrderStoredOrderChef, StoredMenuStoredMenuDeadline
    , StoredOrderStoredOrderMenu, ChefTagRelationChefTagChef
    , StoredOrderStoredOrderCustomer, MealTagRelationMealTagMeal
    , StoredOrderStoredOrderProgress, StoredOrderStoredOrderMeal
    , StoredMealStoredMealMenu, StoredReviewStoredReviewMeal
    , MealIngredientMealIngredientMeal
    )
  , Unique (UniqueChefPermalink, UniqueMealPermalink, UniqueMenuDeadline)
  )

import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Data.Text.Permalink (Permalink)
import Data.IORef (newIORef, readIORef, modifyIORef)
import Data.Time (getCurrentTime, utctDay)
import Data.Time.Calendar (Day)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import Database.Persist (Entity (..), (==.), (=.), (>=.), (!=.))
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Class (selectList, get, getBy, insert, insert_, deleteWhere, update, count)


setCustomer :: AuthToken -> Customer -> AppM Bool
setCustomer authToken Customer{..} = do
  SystemEnv{systemEnvTokenContexts,systemEnvDatabase} <- ask

  case systemEnvTokenContexts of
    TokenContexts{tokenContextAuth} -> do
      mUserId <- liftIO (lookupAccess tokenContextAuth authToken)
      case mUserId of
        Nothing -> pure False
        Just k -> flip runSqlPool systemEnvDatabase $ do
          mCustEnt <- getBy (UniqueCustomer k)
          case mCustEnt of
            Nothing -> do
              custId <- insert (StoredCustomer k customerName customerAddress)
              forM_ customerDiets $ \d -> do
                mDietId <- liftIO (getDietId systemEnvDatabase d)
                case mDietId of
                  Nothing -> pure ()
                  Just dietId -> insert_ (StoredDietPreference custId dietId)
              forM_ customerAllergies $ \i -> do
                mIngId <- liftIO (getStoredIngredientId systemEnvDatabase i)
                case mIngId of
                  Nothing -> pure ()
                  Just ingId -> insert_ (StoredAllergy custId ingId)
              pure True
            Just (Entity custId _) -> do
              update custId
                [ StoredCustomerStoredCustomerName =. customerName
                , StoredCustomerStoredCustomerAddress =. customerAddress
                ]
              oldDiets <- fmap (fmap (\(Entity _ (StoredDietPreference _ d)) -> d))
                        $ selectList [StoredDietPreferenceDietPreferenceOwner ==. custId] []
              newDiets <- fmap catMaybes $ forM customerDiets $ \d ->
                          liftIO (getDietId systemEnvDatabase d)
              let toRemove = Set.fromList oldDiets `Set.difference` Set.fromList newDiets
                  toAdd = Set.fromList newDiets `Set.difference` Set.fromList oldDiets
              forM_ toRemove $ \d -> deleteWhere
                [ StoredDietPreferenceDietPreferenceOwner ==. custId
                , StoredDietPreferenceDietPreferenceDiet ==. d
                ]
              forM_ toAdd $ \d -> insert_ (StoredDietPreference custId d)

              oldAllergys <- fmap (fmap (\(Entity _ (StoredAllergy _ d)) -> d))
                          $ selectList [StoredAllergyAllergyOwner ==. custId] []
              newAllergys <- fmap catMaybes $ forM customerAllergies $ \i ->
                            liftIO (getStoredIngredientId systemEnvDatabase i)
              let toRemove = Set.fromList oldAllergys `Set.difference` Set.fromList newAllergys
                  toAdd = Set.fromList newAllergys `Set.difference` Set.fromList oldAllergys
              forM_ toRemove $ \i -> deleteWhere
                [ StoredAllergyAllergyOwner ==. custId
                , StoredAllergyAllergy ==. i
                ]
              forM_ toAdd $ \i -> insert_ (StoredAllergy custId i)

              pure True


getReview :: StoredReviewId -> AppM (Maybe Review)
getReview reviewId = do
  SystemEnv{systemEnvDatabase} <- ask

  flip runSqlPool systemEnvDatabase $ do
    mReview <- get reviewId
    case mReview of
      Nothing -> pure Nothing
      Just (StoredReview _ _ _ rating submitted heading body images _) ->
        pure $ Just Review
          { reviewRating = rating
          , reviewSubmitted = submitted
          , reviewHeading = heading
          , reviewId = reviewId
          , reviewBody = body
          , reviewImages = images
          }


-- | Returns the set of non-violated diets, and used ingredients per diet
getMealIngredientsDiets :: StoredMealId -> AppM (Maybe ([IngredientName],[Diet]))
getMealIngredientsDiets mealId = do
  SystemEnv{systemEnvDatabase} <- ask

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


getMealSynopsis :: StoredMealId -> AppM (Maybe MealSynopsis)
getMealSynopsis mealId = do
  SystemEnv{systemEnvDatabase,systemEnvReviews} <- ask

  mCont <- flip runSqlPool systemEnvDatabase $ do
    mMeal <- get mealId
    case mMeal of
      Nothing -> pure Nothing
      Just (StoredMeal title permalink _ heading _ _ images price) -> do
        diets <- do
          ings <- selectList [MealIngredientMealIngredientMeal ==. mealId] []
          fmap concat $ forM ings $ \(Entity _ (MealIngredient _ ingId)) ->
            liftIO (getIngredientViolations systemEnvDatabase ingId)
        orders <- count
          [ StoredOrderStoredOrderMeal ==. mealId
          , StoredOrderStoredOrderProgress !=. DeliveredProgress
          ]
        tags <- fmap catMaybes $ do
          xs <- selectList [MealTagRelationMealTagMeal ==. mealId] []
          forM xs $ \(Entity _ (MealTagRelation _ tagId)) ->
            liftIO (getMealTagById systemEnvDatabase tagId)
        pure $ Just (title,permalink,heading,images,price,diets,orders,tags)
  case mCont of
    Nothing -> pure Nothing
    Just (title,permalink,heading,images,price,diets,orders,tags) -> do
      mRating <- liftIO (lookupMealRating systemEnvReviews mealId)
      case mRating of
        Nothing -> pure Nothing
        Just rating ->
          pure $ Just MealSynopsis
            { mealSynopsisTitle = title
            , mealSynopsisPermalink = permalink
            , mealSynopsisHeading = heading
            , mealSynopsisImages = images
            , mealSynopsisRating = rating
            , mealSynopsisOrders = orders
            , mealSynopsisTags = tags
            , mealSynopsisDiets = diets
            , mealSynopsisPrice = price
            }


getChefSynopsis :: StoredChefId -> AppM (Maybe ChefSynopsis)
getChefSynopsis chefId = do
  SystemEnv{systemEnvDatabase,systemEnvReviews} <- ask

  mStoredChef <- flip runSqlPool systemEnvDatabase $ do
    mChef <- get chefId
    case mChef of
      Nothing -> pure Nothing
      Just (StoredChef _ name permalink _ _ avatar) -> do
        tagEnts <- selectList [ChefTagRelationChefTagChef ==. chefId] []
        tags <- fmap catMaybes $ forM tagEnts $ \(Entity _ (ChefTagRelation _ tagId)) ->
          liftIO (getChefTagById systemEnvDatabase tagId)
        orders <- count [StoredOrderStoredOrderChef ==. chefId]
        pure $ Just (name,permalink,avatar,tags,orders)
  case mStoredChef of
    Nothing -> pure Nothing
    Just (name,permalink,avatar,tags,orders) -> do
      mReviews <- liftIO (lookupChefReviews systemEnvReviews chefId)
      case mReviews of
        Nothing -> pure Nothing
        Just (rating,_) ->
          pure $ Just ChefSynopsis
            { chefSynopsisName = name
            , chefSynopsisPermalink = permalink
            , chefSynopsisImage = avatar
            , chefSynopsisRating = rating
            , chefSynopsisOrders = orders
            , chefSynopsisTags = tags
            }


getChefMenuSynopses :: StoredChefId -> AppM [MenuSynopsis]
getChefMenuSynopses chefId = do
  SystemEnv{systemEnvDatabase} <- ask

  flip runSqlPool systemEnvDatabase $ do
    xs <- selectList [StoredMenuStoredMenuAuthor ==. chefId] []
    fmap catMaybes $ forM xs $ \(Entity k (StoredMenu published deadline heading _ images _)) ->
      case published of
        Nothing -> pure Nothing
        Just p -> do
          tagIds <- fmap (fmap (\(Entity _ (MenuTagRelation _ t)) -> t)) $ selectList [MenuTagRelationMenuTagMenu ==. k] []
          tags <- fmap catMaybes $ forM tagIds $ \tagId -> liftIO (getMealTagById systemEnvDatabase tagId)
          pure $ Just MenuSynopsis
            { menuSynopsisPublished = p
            , menuSynopsisDeadline = deadline
            , menuSynopsisHeading = heading
            , menuSynopsisTags = tags
            , menuSynopsisImages = images
            }


getMenuMealSynopses :: StoredMenuId -> AppM [MealSynopsis]
getMenuMealSynopses menuId = do
  SystemEnv{systemEnvDatabase} <- ask

  xs <- flip runSqlPool systemEnvDatabase $
    selectList [StoredMealStoredMealMenu ==. menuId] []
  fmap catMaybes $ forM xs $ \(Entity mealId _) ->
    getMealSynopsis mealId


browseChef :: Permalink -> AppM (Maybe Chef)
browseChef chefPermalink = do
  SystemEnv{systemEnvDatabase,systemEnvReviews} <- ask

  mDeets <- flip runSqlPool systemEnvDatabase $ do
    mChefEnt <- getBy (UniqueChefPermalink chefPermalink)
    case mChefEnt of
      Nothing -> pure Nothing
      Just (Entity chefId (StoredChef _ name permalink bio images _)) -> do
        tagEnts <- selectList [ChefTagRelationChefTagChef ==. chefId] []
        tags <- fmap catMaybes $ forM tagEnts $ \(Entity _ (ChefTagRelation _ tagId)) ->
          liftIO (getChefTagById systemEnvDatabase tagId)

        totalOrders <- count [StoredOrderStoredOrderChef ==. chefId]
        today <- utctDay <$> liftIO getCurrentTime
        activeOrders <- do
          menus <- selectList [StoredMenuStoredMenuDeadline >=. today] []
          countRef <- liftIO (newIORef 0)
          forM_ menus $ \(Entity menuId _) -> do
            n <- count [StoredOrderStoredOrderMenu ==. menuId]
            liftIO (modifyIORef countRef (+ n))
          liftIO (readIORef countRef)
        pure $ Just (chefId,name,permalink,bio,images,tags,totalOrders,activeOrders)
        -- FIXME TODO reviews engine - skimming the top for chefs, but still accum the rating value. Same w/ meals

  case mDeets of
    Nothing -> pure Nothing
    Just (chefId,name,permalink,bio,images,tags,totalOrders,activeOrders) -> do
      meals <- getChefMenuSynopses chefId
      mReviews <- liftIO (lookupChefReviews systemEnvReviews chefId)
      case mReviews of
        Nothing -> pure Nothing
        Just (rating,reviews) ->
          pure $ Just Chef
            { chefName = name
            , chefPermalink = permalink
            , chefImages = images
            , chefBio = bio
            , chefRating = rating
            , chefReviews = getReviewSynopsis <$> reviews
            , chefActiveOrders = activeOrders
            , chefTotalOrders = totalOrders
            , chefTags = tags
            , chefMenus = meals
            }


browseMenu :: Permalink -> Day -> AppM (Maybe Menu)
browseMenu chefPermalink deadline = do
  SystemEnv{systemEnvDatabase} <- ask

  mMenuDeets <- liftIO $ flip runSqlPool systemEnvDatabase $ do
    mChef <- getBy (UniqueChefPermalink chefPermalink)
    case mChef of
      Nothing -> pure Nothing
      Just (Entity chefId _) -> do
        mMenu <- getBy (UniqueMenuDeadline chefId deadline)
        case mMenu of
          Nothing -> pure Nothing
          Just (Entity menuId (StoredMenu mPub _ _ desc _ _)) ->
            case mPub of
              Nothing -> pure Nothing
              Just published -> pure $ Just (menuId,published,desc,chefId)

  case mMenuDeets of
    Nothing -> pure Nothing
    Just (menuId,published,desc,chefId) -> do
      mChef <- getChefSynopsis chefId
      case mChef of
        Nothing -> pure Nothing
        Just chef -> do
          meals <- getMenuMealSynopses menuId
          pure $ Just Menu
            { menuPublished = published
            , menuDeadline = deadline
            , menuDescription = desc
            , menuAuthor = chef
            , menuMeals = meals
            }

browseMeal :: Permalink -> Day -> Permalink -> AppM (Maybe Meal)
browseMeal chefPermalink deadline mealPermalink = do
  SystemEnv{systemEnvReviews,systemEnvDatabase} <- ask

  mStoredMeal <- liftIO $ flip runSqlPool systemEnvDatabase $ do
    mChef <- getBy (UniqueChefPermalink chefPermalink)
    case mChef of
      Nothing -> pure Nothing
      Just (Entity chefId _) -> do
        mMenu <- getBy (UniqueMenuDeadline chefId deadline)
        case mMenu of
          Nothing -> pure Nothing
          Just (Entity menuId _) -> do
            mMeal <- getBy (UniqueMealPermalink menuId mealPermalink)
            case mMeal of
              Nothing -> pure Nothing
              Just (Entity mealId (StoredMeal title permalink _ _ desc inst images price)) -> do
                (tags :: [MealTag]) <- do
                  xs <- selectList [MealTagRelationMealTagMeal ==. mealId] []
                  fmap catMaybes $ forM xs $ \(Entity _ (MealTagRelation _ tagId)) ->
                    liftIO (getMealTagById systemEnvDatabase tagId)
                orders <- count
                  [ StoredOrderStoredOrderMeal ==. mealId
                  , StoredOrderStoredOrderProgress !=. DeliveredProgress
                  ]
                reviewIds <-
                  fmap (fmap (\(Entity reviewId _) -> reviewId)) $
                    selectList [StoredReviewStoredReviewMeal ==. mealId] []
                mRating <- liftIO (lookupMealRating systemEnvReviews mealId)
                case mRating of
                  Nothing -> pure Nothing
                  Just rating ->
                    pure $ Just (mealId,title,permalink,desc,inst,images,tags,orders,rating,reviewIds,price)

  case mStoredMeal of
    Nothing -> pure Nothing
    Just (mealId,title,permalink,desc,inst,images,tags,orders,rating,reviewIds,price) -> do
      reviews <- fmap catMaybes $ forM reviewIds getReview
      mIngDiets <- getMealIngredientsDiets mealId
      case mIngDiets of
        Nothing -> pure Nothing
        Just (ings,diets) -> do
          ings' <- fmap catMaybes $ liftIO $
            forM ings $ getIngredientByName systemEnvDatabase
          pure $ Just Meal
            { mealTitle = title
            , mealPermalink = permalink
            , mealDescription = desc
            , mealInstructions = inst
            , mealImages = images
            , mealIngredients = ings'
            , mealDiets = diets
            , mealTags = tags
            , mealOrders = orders
            , mealRating = rating
            , mealReviews = getReviewSynopsis <$> reviews
            , mealPrice = price
            }


-- addToCart :: Permalink -> Day -> Permalink -> Int -> AppM Bool

-- checkout :: ?

getOrders :: AuthToken -> AppM (Maybe [Order])
getOrders authToken = do
  SystemEnv{systemEnvTokenContexts,systemEnvDatabase} <- ask

  case systemEnvTokenContexts of
    TokenContexts{tokenContextAuth} -> do
      mUserId <- liftIO (lookupAccess tokenContextAuth authToken)
      case mUserId of
        Nothing -> pure Nothing
        Just k -> do
          xs <- flip runSqlPool systemEnvDatabase $
            selectList [StoredOrderStoredOrderCustomer ==. k] []
          fmap (Just . catMaybes) $
            forM xs $ \(Entity _ (StoredOrder _ mealId _ _ vol progress time)) -> do
              mMealSynopsis <- getMealSynopsis mealId
              case mMealSynopsis of
                Nothing -> pure Nothing
                Just meal ->
                  pure $ Just Order
                    { orderMeal = meal
                    , orderProgress = progress
                    , orderTime = time
                    , orderVolume = vol
                    }
