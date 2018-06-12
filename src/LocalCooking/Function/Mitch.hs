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
  , Order (..), CartEntry (..)
  , getReviewSynopsis
  )
import LocalCooking.Function.Semantics
  ( getMealIngredientsDiets, getMealTags, getMenuTags, getChefTags
  , assignAllergies, assignDiets, getCustDiets, getCustAllergies)
import LocalCooking.Function.System (SystemM, SystemEnv (..), getUserId, guardRole, getSystemEnv)
import LocalCooking.Function.System.Review (lookupChefReviews, lookupMealRating)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Order (OrderProgress (DeliveredProgress))
import LocalCooking.Common.Rating (Rating)
import qualified LocalCooking.Common.User.Role as UserRole
import LocalCooking.Database.Query.IngredientDiet (getIngredientByName)
import LocalCooking.Database.Query.Semantics (getMealId)
import LocalCooking.Database.Schema.User.Customer
  ( StoredCustomer (..)
  , EntityField
    ( StoredCustomerStoredCustomerAddress
    , StoredCustomerStoredCustomerName
    )
  , Unique (UniqueCustomer))
import LocalCooking.Database.Schema.Semantics
  ( StoredMenu (..), StoredChef (..), StoredOrder (..), StoredMeal (..), StoredReview (..)
  , StoredChefId, StoredMealId, StoredMenuId, StoredReviewId, StoredOrderId
  , CartRelation (..)
  , EntityField
    ( StoredMenuStoredMenuAuthor
    , StoredOrderStoredOrderChef, StoredMenuStoredMenuDeadline
    , StoredOrderStoredOrderMenu
    , StoredOrderStoredOrderCustomer
    , StoredOrderStoredOrderProgress, StoredOrderStoredOrderMeal
    , StoredMealStoredMealMenu, StoredReviewStoredReviewMeal
    , CartRelationCartRelationAdded, CartRelationCartRelationVolume
    , CartRelationCartRelationCustomer
    )
  , Unique (UniqueChefPermalink, UniqueMealPermalink, UniqueMenuDeadline, UniqueCartRelation)
  )

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.Permalink (Permalink)
import Data.Text.Markdown (MarkdownText)
import Data.IORef (newIORef, readIORef, modifyIORef)
import Data.Image.Source (ImageSource)
import Data.Time (getCurrentTime, utctDay)
import Data.Time.Calendar (Day)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.Persist (Entity (..), (==.), (=.), (>=.), (!=.))
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Class (selectList, get, getBy, insert, insert_, update, count)




setCustomer :: AuthToken -> Customer -> SystemM Bool
setCustomer authToken Customer{..} = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure False
    Just userId -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      mCust <- liftIO $ flip runSqlPool systemEnvDatabase $ do
        mCustEnt <- getBy (UniqueCustomer userId)
        case mCustEnt of
          Nothing -> do
            custId <- insert (StoredCustomer userId customerName customerAddress)
            pure (Just custId)
          Just (Entity custId _) -> do
            update custId
              [ StoredCustomerStoredCustomerName =. customerName
              , StoredCustomerStoredCustomerAddress =. customerAddress
              ]
            pure (Just custId)
      case mCust of
        Nothing -> pure False
        Just custId -> liftIO $ do
          assignDiets systemEnvDatabase custId customerDiets
          assignAllergies systemEnvDatabase custId customerAllergies
          pure True


getCustomer :: AuthToken -> SystemM (Maybe Customer)
getCustomer authToken = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure Nothing
    Just userId -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        mCustEnt <- getBy (UniqueCustomer userId)
        case mCustEnt of
          Nothing -> pure Nothing
          Just (Entity custId (StoredCustomer _ name address)) -> do
            mDiets <- liftIO $ getCustDiets systemEnvDatabase custId
            case mDiets of
              Nothing -> pure Nothing
              Just diets -> do
                mAllergies <- liftIO $ getCustAllergies systemEnvDatabase custId
                case mAllergies of
                  Nothing -> pure Nothing
                  Just allergies ->
                    pure $ Just $ Customer
                      name
                      address
                      diets
                      allergies


submitReview :: AuthToken -> StoredOrderId -> Rating -> Text
             -> MarkdownText -> [ImageSource] -> SystemM (Maybe StoredReviewId)
submitReview authToken orderId rating heading body images = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure Nothing
    Just userId -> do
      ok <- guardRole userId UserRole.Customer
      if not ok
        then pure Nothing
        else do
          SystemEnv{systemEnvDatabase} <- getSystemEnv
          liftIO $ flip runSqlPool systemEnvDatabase $ do
            mCust <- getBy (UniqueCustomer userId)
            case mCust of
              Nothing -> pure Nothing
              Just (Entity custId _) -> do
                mOrder <- get orderId
                case mOrder of
                  Nothing -> pure Nothing
                  Just (StoredOrder custId' mealId _ chefId _ _ _)
                    | custId /= custId' -> pure Nothing
                    | otherwise -> do
                        now <- liftIO getCurrentTime
                        reviewId <- insert $ StoredReview
                          orderId
                          chefId
                          mealId
                          custId
                          rating
                          now
                          heading
                          body
                          images
                        pure (Just reviewId)



getReview :: StoredReviewId -> SystemM (Maybe Review)
getReview reviewId = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv

  flip runSqlPool systemEnvDatabase $ do
    mReview <- get reviewId
    case mReview of
      Nothing -> pure Nothing
      Just (StoredReview _ _ _ _ rating submitted heading body images) ->
        pure $ Just Review
          { reviewRating = rating
          , reviewSubmitted = submitted
          , reviewHeading = heading
          , reviewId = reviewId
          , reviewBody = body
          , reviewImages = images
          }



getMealSynopsis :: StoredMealId -> SystemM (Maybe MealSynopsis)
getMealSynopsis mealId = do
  SystemEnv{systemEnvDatabase,systemEnvReviews} <- getSystemEnv

  mCont <- flip runSqlPool systemEnvDatabase $ do
    mMeal <- get mealId
    case mMeal of
      Nothing -> pure Nothing
      Just (StoredMeal title permalink _ heading _ _ images price) -> do
        mTags <- liftIO (getMealTags systemEnvDatabase mealId)
        case mTags of
          Nothing -> pure Nothing
          Just tags -> do
            mIngDiets <- liftIO (getMealIngredientsDiets systemEnvDatabase mealId)
            case mIngDiets of
              Nothing -> pure Nothing
              Just (_,diets) -> do
                orders <- count
                  [ StoredOrderStoredOrderMeal ==. mealId
                  , StoredOrderStoredOrderProgress !=. DeliveredProgress
                  ]
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


getChefSynopsis :: StoredChefId -> SystemM (Maybe ChefSynopsis)
getChefSynopsis chefId = do
  SystemEnv{systemEnvDatabase,systemEnvReviews} <- getSystemEnv

  mStoredChef <- flip runSqlPool systemEnvDatabase $ do
    mChef <- get chefId
    case mChef of
      Nothing -> pure Nothing
      Just (StoredChef _ name permalink _ _ avatar) -> do
        mTags <- liftIO (getChefTags systemEnvDatabase chefId)
        case mTags of
          Nothing -> pure Nothing
          Just tags -> do
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


getChefMenuSynopses :: StoredChefId -> SystemM (Maybe [MenuSynopsis])
getChefMenuSynopses chefId = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv

  flip runSqlPool systemEnvDatabase $ do
    mChef <- get chefId
    case mChef of
      Nothing -> pure Nothing
      Just _ -> do
        xs <- selectList [StoredMenuStoredMenuAuthor ==. chefId] []
        fmap (Just . catMaybes) $ forM xs $ \(Entity menuId (StoredMenu published deadline heading _ images _)) ->
          case published of
            Nothing -> pure Nothing
            Just p -> do
              mTags <- liftIO (getMenuTags systemEnvDatabase menuId)
              case mTags of
                Nothing -> pure Nothing
                Just tags ->
                  pure $ Just MenuSynopsis
                    { menuSynopsisPublished = p
                    , menuSynopsisDeadline = deadline
                    , menuSynopsisHeading = heading
                    , menuSynopsisTags = tags
                    , menuSynopsisImages = images
                    }


getMenuMealSynopses :: StoredMenuId -> SystemM (Maybe [MealSynopsis])
getMenuMealSynopses menuId = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv

  mXs <- flip runSqlPool systemEnvDatabase $ do
    mMenu <- get menuId
    case mMenu of
      Nothing -> pure Nothing
      Just _ -> Just <$> selectList [StoredMealStoredMealMenu ==. menuId] []
  case mXs of
    Nothing -> pure Nothing
    Just xs -> fmap (Just . catMaybes) $ forM xs $ \(Entity mealId _) ->
                  getMealSynopsis mealId


browseChef :: Permalink -> SystemM (Maybe Chef)
browseChef chefPermalink = do
  SystemEnv{systemEnvDatabase,systemEnvReviews} <- getSystemEnv
  mDeets <- flip runSqlPool systemEnvDatabase $ do
    mChefEnt <- getBy (UniqueChefPermalink chefPermalink)
    case mChefEnt of
      Nothing -> pure Nothing
      Just (Entity chefId (StoredChef _ name permalink bio images _)) -> do
        mTags <- liftIO (getChefTags systemEnvDatabase chefId)
        case mTags of
          Nothing -> pure Nothing
          Just tags -> do
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

  case mDeets of
    Nothing -> pure Nothing
    Just (chefId,name,permalink,bio,images,tags,totalOrders,activeOrders) -> do
      mMeals <- getChefMenuSynopses chefId
      case mMeals of
        Nothing -> pure Nothing
        Just meals -> do
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


browseMenu :: Permalink -> Day -> SystemM (Maybe Menu)
browseMenu chefPermalink deadline = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv

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
          mMeals <- getMenuMealSynopses menuId
          case mMeals of
            Nothing -> pure Nothing
            Just meals ->
              pure $ Just Menu
                { menuPublished = published
                , menuDeadline = deadline
                , menuDescription = desc
                , menuAuthor = chef
                , menuMeals = meals
                }

browseMeal :: Permalink -> Day -> Permalink -> SystemM (Maybe Meal)
browseMeal chefPermalink deadline mealPermalink = do
  SystemEnv{systemEnvReviews,systemEnvDatabase} <- getSystemEnv

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
                mIngDiets <- liftIO (getMealIngredientsDiets systemEnvDatabase mealId)
                case mIngDiets of
                  Nothing -> pure Nothing
                  Just (ings,diets) -> do
                    mTags <- liftIO (getMealTags systemEnvDatabase mealId)
                    case mTags of
                      Nothing -> pure Nothing
                      Just tags -> do
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
                            pure $ Just (title,permalink,desc,inst,images,tags,orders,rating,reviewIds,price,ings,diets)

  case mStoredMeal of
    Nothing -> pure Nothing
    Just (title,permalink,desc,inst,images,tags,orders,rating,reviewIds,price,ings,diets) -> do
      reviews <- fmap catMaybes $ forM reviewIds getReview
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


getCart :: AuthToken -> SystemM (Maybe [CartEntry])
getCart authToken = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure Nothing
    Just userId -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $
        fmap (Just . fmap (\(Entity _ (CartRelation _ mealId vol time)) -> CartEntry mealId vol time))
          $ selectList [CartRelationCartRelationCustomer ==. userId] []



addToCart :: AuthToken -> Permalink -> Day -> Permalink -> Int -> SystemM Bool
addToCart authToken chefPermalink deadline mealPermalink vol = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure False
    Just userId -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      mMealId <- liftIO (getMealId systemEnvDatabase chefPermalink deadline mealPermalink)
      case mMealId of
        Nothing -> pure False
        Just mealId -> do
          now <- liftIO getCurrentTime
          liftIO $ flip runSqlPool systemEnvDatabase $ do
            mEntry <- getBy (UniqueCartRelation userId mealId)
            case mEntry of
              Nothing ->
                insert_ (CartRelation userId mealId vol now)
              Just (Entity cartId (CartRelation _ _ oldVol _)) ->
                update cartId
                  [ CartRelationCartRelationVolume =. (vol + oldVol)
                  , CartRelationCartRelationAdded =. now
                  ]
          pure True

-- checkout :: ?

getOrders :: AuthToken -> SystemM (Maybe [Order])
getOrders authToken = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure Nothing
    Just userId -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      mXs <- flip runSqlPool systemEnvDatabase $ do
        mCust <- getBy (UniqueCustomer userId)
        case mCust of
          Nothing -> pure Nothing
          Just (Entity custId _) ->
            Just <$> selectList [StoredOrderStoredOrderCustomer ==. custId] []
      case mXs of
        Nothing -> pure Nothing
        Just xs -> fmap (Just . catMaybes) $
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


-- searchMealTags :: Text -> SystemM [MealTag]
-- searchMealTags term =
--   where
    
