{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  , ScopedTypeVariables
  , OverloadedStrings
  #-}

module LocalCooking.Function.Mitch
  ( ValidateCustomerError (..), validateCustomer
  , getCustomer, setCustomer, unsafeStoreCustomer
  , getDiets, setDiets, getAllergies, setAllergies
  , getReview, submitReview
  , getChefSynopsis, getChefMenuSynopses, getMenuMealSynopses
  , browseChef, browseMenu, browseMeal
  , getCart, addToCart
  , getOrders
  ) where

import LocalCooking.Semantics.Mitch
  ( SetCustomer (..), CustomerValid (..), Diets (Diets), Allergies (Allergies)
  , Review (..), SubmitReview (..)
  , Chef (..), ChefSynopsis (..)
  , MenuSynopsis (..), Menu (..)
  , MealSynopsis (..), Meal (..)
  , Order (..), CartEntry (..)
  , getReviewSynopsis
  )
import LocalCooking.Semantics.ContentRecord
  ( ContentRecord (ProfileRecord), ProfileRecord (ProfileRecordCustomer)
  , contentRecordVariant
  )
import LocalCooking.Function.Semantics
  ( getMealIngredientsDiets, getMealTags, getMenuTags, getChefTags
  , assignAllergies, assignDiets, getCustDiets, getCustAllergies)
import LocalCooking.Function.System (SystemM, SystemEnv (..), getUserId, getSystemEnv)
import LocalCooking.Function.System.Review (lookupChefReviews, lookupMealRating)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Order (OrderProgress (DeliveredProgress))
import LocalCooking.Common.Rating (Rating)
import qualified LocalCooking.Common.User.Role as UserRole
import LocalCooking.Database.Schema
  ( getIngredientByName, getMealId, hasRole
  , StoredCustomer (..)
  , StoredMenu (..), StoredChef (..), StoredOrder (..), StoredMeal (..), StoredReview (..)
  , StoredChefId, StoredMealId, StoredMenuId, StoredReviewId, StoredOrderId
  , CartRelation (..)
  , EntityField
    ( StoredMenuAuthor
    , StoredOrderChef, StoredMenuDeadline
    , StoredOrderMenu
    , StoredOrderCustomer
    , StoredOrderProgress, StoredOrderMeal
    , StoredMealMenu, StoredReviewMeal
    , CartRelationAdded, CartRelationVolume
    , CartRelationCustomer
    , StoredCustomerAddress
    , StoredCustomerName
    )
  , Unique
    ( UniqueChefPermalink, UniqueMealPermalink
    , UniqueMenuDeadline, UniqueCartRelation
    , UniqueCustomer
    )
  )
import LocalCooking.Database.Schema.Content
  ( StoredRecordSubmission (..)
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
import Control.Monad.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.Persist (Entity (..), (==.), (=.), (>=.), (!=.))
import Database.Persist.Sql (SqlBackend, runSqlPool)
import Database.Persist.Class (selectList, get, getBy, insert, insert_, update, count)




data ValidateCustomerError
  = CustomerInvalidNoName
  | CustomerInvalidNoAddress


-- | Turns user-supplied SetCustomer partial structure into a valid one, checking
--   for uniqueness.
validateCustomer :: SetCustomer -> ReaderT SqlBackend IO (Either ValidateCustomerError CustomerValid)
validateCustomer SetCustomer{..} =
  case setCustomerName of
    Nothing -> pure (Left CustomerInvalidNoName)
    Just name -> case setCustomerAddress of
      Nothing -> pure (Left CustomerInvalidNoAddress)
      Just address -> pure $ Right CustomerValid
        { customerValidName = name
        , customerValidAddress = address
        }


-- | Witness the stored customer profile of a logged-in user
getCustomer :: AuthToken -> SystemM (Maybe CustomerValid)
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
          Just (Entity _ (StoredCustomer _ name address)) ->
            pure $ Just $ CustomerValid
              name
              address


-- | Physically store the content of a `CustomerValid` data-view into the database
unsafeStoreCustomer :: AuthToken -> CustomerValid -> SystemM Bool
unsafeStoreCustomer authToken CustomerValid{..} = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure False
    Just userId -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        mCustEnt <- getBy (UniqueCustomer userId)
        case mCustEnt of
          Nothing -> do
            insert_ (StoredCustomer userId customerValidName customerValidAddress)
            pure True
          Just (Entity custId _) -> do
            update custId
              [ StoredCustomerName =. customerValidName
              , StoredCustomerAddress =. customerValidAddress
              ]
            pure True


-- | Marshall a user-supplied customer profile into the content submission system
setCustomer :: AuthToken -> SetCustomer -> SystemM Bool
setCustomer authToken setCustomer' = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure False
    Just userId -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        now <- liftIO getCurrentTime
        let record = ProfileRecord (ProfileRecordCustomer setCustomer')
        insert_ $ StoredRecordSubmission userId now record (contentRecordVariant record)
        pure True


-- | Physically store the diets dictated by the Diets data-view
setDiets :: AuthToken -> Diets -> SystemM Bool
setDiets authToken (Diets ds) = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure False
    Just userId -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        mCustEnt <- getBy (UniqueCustomer userId)
        mCust <- case mCustEnt of
          Nothing -> pure Nothing
          Just (Entity custId _) -> pure (Just custId)
        case mCust of
          Nothing -> pure False
          Just custId -> do
            assignDiets custId ds
            pure True


-- | Witness the Diets data-view associated with a customer
getDiets :: AuthToken -> SystemM (Maybe Diets)
getDiets authToken = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure Nothing
    Just userId -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        mCustEnt <- getBy (UniqueCustomer userId)
        case mCustEnt of
          Nothing -> pure Nothing
          Just (Entity custId _) -> do
            mDiets <- getCustDiets custId
            case mDiets of
              Nothing -> pure Nothing
              Just diets -> pure $ Just $ Diets diets


-- | Physically store the allergies dictated by the Allergies data-view
setAllergies :: AuthToken -> Allergies -> SystemM Bool
setAllergies authToken (Allergies as) = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure False
    Just userId -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        mCustEnt <- getBy (UniqueCustomer userId)
        mCust <- case mCustEnt of
          Nothing -> pure Nothing
          Just (Entity custId _) -> pure (Just custId)
        case mCust of
          Nothing -> pure False
          Just custId -> do
            assignAllergies custId as
            pure True


-- | Witness the Allergies data-view associated with a customer
getAllergies :: AuthToken -> SystemM (Maybe Allergies)
getAllergies authToken = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure Nothing
    Just userId -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        mCustEnt <- getBy (UniqueCustomer userId)
        case mCustEnt of
          Nothing -> pure Nothing
          Just (Entity custId _) -> do
            mAllergies <- getCustAllergies custId
            case mAllergies of
              Nothing -> pure Nothing
              Just allergies -> pure $ Just $ Allergies allergies



-- | Physically store a customer's review
--   FIXME NewReview data-view?
submitReview :: AuthToken -> SubmitReview -> SystemM (Maybe StoredReviewId)
submitReview authToken (SubmitReview orderId rating heading body images) = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure Nothing
    Just userId -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        ok <- hasRole userId UserRole.Customer
        if not ok
          then pure Nothing
          else do
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



-- | Witness a customer's review
--   FIXME GetReview data-view?
getReview :: StoredReviewId -> ReaderT SqlBackend IO (Maybe Review)
getReview reviewId = do
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


-- | Witness a MealSynopsis data-view
getMealSynopsis :: StoredMealId -> SystemM (Maybe MealSynopsis)
getMealSynopsis mealId = do
  SystemEnv{systemEnvDatabase,systemEnvReviews} <- getSystemEnv

  mCont <- liftIO $ flip runSqlPool systemEnvDatabase $ do
    mMeal <- get mealId
    case mMeal of
      Nothing -> pure Nothing
      Just (StoredMeal title permalink _ heading _ _ images price) -> do
        mTags <- getMealTags mealId
        case mTags of
          Nothing -> pure Nothing
          Just tags -> do
            mIngDiets <- getMealIngredientsDiets mealId
            case mIngDiets of
              Nothing -> pure Nothing
              Just (_,diets) -> do
                orders <- count
                  [ StoredOrderMeal ==. mealId
                  , StoredOrderProgress !=. DeliveredProgress
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


-- | Witness a ChefSynopsis data-view
getChefSynopsis :: StoredChefId -> SystemM (Maybe ChefSynopsis)
getChefSynopsis chefId = do
  SystemEnv{systemEnvDatabase,systemEnvReviews} <- getSystemEnv

  mStoredChef <- liftIO $ flip runSqlPool systemEnvDatabase $ do
    mChef <- get chefId
    case mChef of
      Nothing -> pure Nothing
      Just (StoredChef _ name permalink _ _ avatar) -> do
        mTags <- getChefTags chefId
        case mTags of
          Nothing -> pure Nothing
          Just tags -> do
            orders <- count [StoredOrderChef ==. chefId]
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


-- | Witness all MenuSynopsis data-views belonging to a chef
getChefMenuSynopses :: StoredChefId -> ReaderT SqlBackend IO (Maybe [MenuSynopsis])
getChefMenuSynopses chefId = do
  mChef <- get chefId
  case mChef of
    Nothing -> pure Nothing
    Just _ -> do
      xs <- selectList [StoredMenuAuthor ==. chefId] []
      fmap (Just . catMaybes) $ forM xs $ \(Entity menuId (StoredMenu published deadline heading _ images _)) ->
        case published of
          Nothing -> pure Nothing
          Just p -> do
            mTags <- getMenuTags menuId
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


-- | Witness all MealSynopsis data-views belonging to a Menu
getMenuMealSynopses :: StoredMenuId -> SystemM (Maybe [MealSynopsis])
getMenuMealSynopses menuId = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv

  mXs <- flip runSqlPool systemEnvDatabase $ do
    mMenu <- get menuId
    case mMenu of
      Nothing -> pure Nothing
      Just _ -> Just <$> selectList [StoredMealMenu ==. menuId] []
  case mXs of
    Nothing -> pure Nothing
    Just xs -> fmap (Just . catMaybes) $ forM xs $ \(Entity mealId _) ->
                  getMealSynopsis mealId


-- | Witness a Chef data-view
-- FIXME really dirty
browseChef :: Permalink -> SystemM (Maybe Chef)
browseChef chefPermalink = do
  SystemEnv{systemEnvDatabase,systemEnvReviews} <- getSystemEnv

  liftIO $ flip runSqlPool systemEnvDatabase $ do
    mChefEnt <- getBy (UniqueChefPermalink chefPermalink)
    case mChefEnt of
      Nothing -> pure Nothing
      Just (Entity chefId (StoredChef _ name permalink bio images _)) -> do
        mTags <- getChefTags chefId
        case mTags of
          Nothing -> pure Nothing
          Just tags -> do
            totalOrders <- count [StoredOrderChef ==. chefId]
            today <- utctDay <$> liftIO getCurrentTime
            activeOrders <- do
              menus <- selectList [StoredMenuDeadline >=. today] []
              countRef <- liftIO (newIORef 0)
              forM_ menus $ \(Entity menuId _) -> do
                n <- count [StoredOrderMenu ==. menuId]
                liftIO (modifyIORef countRef (+ n))
              liftIO (readIORef countRef)
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


-- | Witness a Menu data-view
-- FIXME really dirty
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


-- | Witness a Meal data-view
-- FIXME really dirty
browseMeal :: Permalink -> Day -> Permalink -> SystemM (Maybe Meal)
browseMeal chefPermalink deadline mealPermalink = do
  SystemEnv{systemEnvReviews,systemEnvDatabase} <- getSystemEnv

  liftIO $ flip runSqlPool systemEnvDatabase $ do
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
                mIngDiets <- getMealIngredientsDiets mealId
                case mIngDiets of
                  Nothing -> pure Nothing
                  Just (ings,diets) -> do
                    mTags <- getMealTags mealId
                    case mTags of
                      Nothing -> pure Nothing
                      Just tags -> do
                        orders <- count
                          [ StoredOrderMeal ==. mealId
                          , StoredOrderProgress !=. DeliveredProgress
                          ]
                        reviewIds <-
                          fmap (fmap (\(Entity reviewId _) -> reviewId)) $
                            selectList [StoredReviewMeal ==. mealId] []
                        mRating <- liftIO (lookupMealRating systemEnvReviews mealId)
                        case mRating of
                          Nothing -> pure Nothing
                          Just rating -> do
                            reviews <- fmap catMaybes $ forM reviewIds getReview
                            ings' <- fmap catMaybes $ forM ings getIngredientByName
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


-- | Witness all CartEntry data-views belonging to a login session's customer
getCart :: AuthToken -> SystemM (Maybe [CartEntry])
getCart authToken = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure Nothing
    Just userId -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $
        fmap (Just . fmap (\(Entity _ (CartRelation _ mealId vol time)) -> CartEntry mealId vol time))
          $ selectList [CartRelationCustomer ==. userId] []


-- | Add a meal to a customer's cart
addToCart :: AuthToken -> Permalink -> Day -> Permalink -> Int -> SystemM Bool
addToCart authToken chefPermalink deadline mealPermalink vol = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure False
    Just userId -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        mMealId <- getMealId chefPermalink deadline mealPermalink
        case mMealId of
          Nothing -> pure False
          Just mealId -> do
            now <- liftIO getCurrentTime
            mEntry <- getBy (UniqueCartRelation userId mealId)
            case mEntry of
              Nothing ->
                insert_ (CartRelation userId mealId vol now)
              Just (Entity cartId (CartRelation _ _ oldVol _)) ->
                update cartId
                  [ CartRelationVolume =. (vol + oldVol)
                  , CartRelationAdded =. now
                  ]
            pure True

-- checkout :: ?

-- | Witness all Order data-views belonging to a login-session's customer
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
            Just <$> selectList [StoredOrderCustomer ==. custId] []
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
