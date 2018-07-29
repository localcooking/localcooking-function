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
  , CustomerExists (..), ReviewExists (..), OrderExists (..), MealExists (..)
  , MenuExists (..), CustomerUnique (..)
  , MealUnique (..), MenuUnique (..), MenuPublished (..), RatingExists (..)
  , menuExistsToMaybe, mealExistsToMaybe, ratingExistsToMaybe, reviewExistsToMaybe
  )
import LocalCooking.Semantics.Chef
  ( ChefExists (..), ChefUnique (..))
import LocalCooking.Semantics.User
  ( UserExists (..), HasRole (..))
import LocalCooking.Semantics.Tag
  ( tagExistsToMaybe)
import LocalCooking.Semantics.ContentRecord
  ( ContentRecord (ProfileRecord), ProfileRecord (ProfileRecordCustomer)
  , contentRecordVariant
  )
import LocalCooking.Function.Semantics
  ( getMealIngredientsDiets, getMealTags, getMenuTags, getChefTags
  , assignAllergies, assignDiets, getCustDiets, getCustAllergies)
import LocalCooking.Function.System (SystemM, SystemEnv (..), getSystemEnv, liftDb)
import LocalCooking.Function.System.Review (lookupChefReviews, lookupMealRating)
import LocalCooking.Function.User (getUserId, verifyRole)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Order (OrderProgress (DeliveredProgress))
import LocalCooking.Common.User.Role (UserRole (Customer))
import LocalCooking.Database.Schema
  ( getIngredientByName, getMealId, hasRole
  , StoredCustomer (..)
  , StoredMenu (..), StoredChef (..), StoredOrder (..), StoredMeal (..), StoredReview (..)
  , StoredChefId, StoredMealId, StoredMenuId, StoredReviewId, StoredUserId
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
import Data.Text.Permalink (Permalink)
import Data.IORef (newIORef, readIORef, modifyIORef)
import Data.Time (getCurrentTime, utctDay)
import Data.Time.Calendar (Day)
import Data.Aeson.JSONUnit (JSONUnit (..))
import Control.Monad (forM, forM_, (<=<))
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
getCustomer :: AuthToken
            -> SystemM
               ( UserExists
                 ( CustomerExists CustomerValid))
getCustomer authToken = do
  mUserId <- getUserId authToken
  case mUserId of
    UserDoesntExist -> pure UserDoesntExist
    UserExists userId -> fmap UserExists $ liftDb $ do
      mCustEnt <- getBy (UniqueCustomer userId)
      case mCustEnt of
        Nothing -> pure CustomerDoesntExist
        Just (Entity _ (StoredCustomer _ name address)) ->
          pure $ CustomerExists $ CustomerValid
            name
            address


-- FIXME align with authentication policy
-- | Physically store the content of a `CustomerValid` data-view into the database
unsafeStoreCustomer :: StoredUserId -> CustomerValid -> SystemM ()
unsafeStoreCustomer userId CustomerValid{..} = do
  liftDb $ do
    mCustEnt <- getBy (UniqueCustomer userId)
    case mCustEnt of
      Nothing -> do
        insert_ (StoredCustomer userId customerValidName customerValidAddress)
      Just (Entity custId _) -> do
        update custId
          [ StoredCustomerName =. customerValidName
          , StoredCustomerAddress =. customerValidAddress
          ]


-- | Marshall a user-supplied customer profile into the content submission system
setCustomer :: AuthToken -> SetCustomer -> SystemM (UserExists JSONUnit)
setCustomer authToken setCustomer' = do
  mUserId <- getUserId authToken
  case mUserId of
    UserDoesntExist -> pure UserDoesntExist
    UserExists userId -> fmap UserExists $ liftDb $ do
      now <- liftIO getCurrentTime
      let record = ProfileRecord (ProfileRecordCustomer setCustomer')
      insert_ $ StoredRecordSubmission userId now record (contentRecordVariant record)
      pure JSONUnit


-- | Physically store the diets dictated by the Diets data-view
setDiets :: AuthToken -> Diets -> SystemM (UserExists (CustomerUnique JSONUnit))
setDiets authToken (Diets ds) = do
  mUserId <- getUserId authToken
  case mUserId of
    UserDoesntExist -> pure UserDoesntExist
    UserExists userId -> fmap UserExists $ liftDb $ do
      mCust <- do
        mCustEnt <- getBy (UniqueCustomer userId)
        case mCustEnt of
          Nothing -> pure Nothing
          Just (Entity custId _) -> pure (Just custId)
      case mCust of
        Nothing -> pure CustomerNotUnique
        Just custId -> fmap CustomerUnique $ do
          assignDiets custId ds
          pure JSONUnit


-- | Witness the Diets data-view associated with a customer
getDiets :: AuthToken
         -> SystemM
            ( UserExists
              ( CustomerUnique
                ( CustomerExists Diets)))
getDiets authToken = do
  mUserId <- getUserId authToken
  case mUserId of
    UserDoesntExist -> pure UserDoesntExist
    UserExists userId -> fmap UserExists $ liftDb $ do
      mCustEnt <- getBy (UniqueCustomer userId)
      case mCustEnt of
        Nothing -> pure CustomerNotUnique
        Just (Entity custId _) -> fmap CustomerUnique $ do
          mDiets <- getCustDiets custId
          case mDiets of
            CustomerDoesntExist -> pure CustomerDoesntExist
            CustomerExists diets -> pure $ CustomerExists $ Diets diets


-- | Physically store the allergies dictated by the Allergies data-view
setAllergies :: AuthToken -> Allergies -> SystemM (UserExists (CustomerUnique JSONUnit))
setAllergies authToken (Allergies as) = do
  mUserId <- getUserId authToken
  case mUserId of
    UserDoesntExist -> pure UserDoesntExist
    UserExists userId -> fmap UserExists $ liftDb $ do
      mCust <- do
        mCustEnt <- getBy (UniqueCustomer userId)
        case mCustEnt of
          Nothing -> pure Nothing
          Just (Entity custId _) -> pure (Just custId)
      case mCust of
        Nothing -> pure CustomerNotUnique
        Just custId -> do
          assignAllergies custId as
          pure (CustomerUnique JSONUnit)


-- | Witness the Allergies data-view associated with a customer
getAllergies :: AuthToken
             -> SystemM
                ( UserExists
                  ( CustomerUnique
                    ( CustomerExists Allergies)))
getAllergies authToken = do
  mUserId <- getUserId authToken
  case mUserId of
    UserDoesntExist -> pure UserDoesntExist
    UserExists userId -> fmap UserExists $ liftDb $ do
      mCustEnt <- getBy (UniqueCustomer userId)
      case mCustEnt of
        Nothing -> pure CustomerNotUnique
        Just (Entity custId _) -> fmap CustomerUnique $ do
          mAllergies <- getCustAllergies custId
          case mAllergies of
            CustomerDoesntExist -> pure CustomerDoesntExist
            CustomerExists allergies -> pure $ CustomerExists $ Allergies allergies



-- | Physically store a customer's review
submitReview :: AuthToken
             -> SubmitReview
             -> SystemM
                ( UserExists
                  ( HasRole
                    ( CustomerUnique
                      ( OrderExists StoredReviewId))))
submitReview authToken (SubmitReview orderId rating heading body images) =
  verifyRole Customer authToken $ \userId -> liftDb $ do
    mCust <- getBy (UniqueCustomer userId)
    case mCust of
      Nothing -> pure CustomerNotUnique
      Just (Entity custId _) -> fmap CustomerUnique $ do
        mOrder <- get orderId
        case mOrder of
          Nothing -> pure OrderDoesntExist
          Just (StoredOrder custId' mealId _ chefId _ _ _)
            | custId /= custId' -> pure OrderDoesntExist
            | otherwise -> fmap OrderExists $ do
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
                pure reviewId



-- | Witness a customer's review
getReview :: StoredReviewId -> ReaderT SqlBackend IO (ReviewExists Review)
getReview reviewId = do
  mReview <- get reviewId
  case mReview of
    Nothing -> pure ReviewDoesntExist
    Just (StoredReview _ _ _ _ rating submitted heading body images) ->
      pure $ ReviewExists Review
        { reviewRating = rating
        , reviewSubmitted = submitted
        , reviewHeading = heading
        , reviewId = reviewId
        , reviewBody = body
        , reviewImages = images
        }


-- | Witness a MealSynopsis data-view
getMealSynopsis :: StoredMealId
                -> SystemM
                   ( MealExists
                     ( RatingExists MealSynopsis))
getMealSynopsis mealId = do
  SystemEnv{systemEnvReviews} <- getSystemEnv

  mCont <- liftDb $ do
    mMeal <- get mealId
    case mMeal of
      Nothing -> pure MealDoesntExist
      Just (StoredMeal title permalink _ heading _ _ images price) -> do
        mTags <- getMealTags mealId
        case mTags of
          MealDoesntExist -> pure MealDoesntExist
          MealExists tags -> do
            mIngDiets <- getMealIngredientsDiets mealId
            case mIngDiets of
              MealDoesntExist -> pure MealDoesntExist
              MealExists (_,diets) -> fmap MealExists $ do
                orders <- count
                  [ StoredOrderMeal ==. mealId
                  , StoredOrderProgress !=. DeliveredProgress
                  ]
                pure (title,permalink,heading,images,price,diets,orders,tags)
  case mCont of
    MealDoesntExist -> pure MealDoesntExist
    MealExists (title,permalink,heading,images,price,diets,orders,tags) -> fmap MealExists $ do
      mRating <- liftIO (lookupMealRating systemEnvReviews mealId)
      case mRating of
        Nothing -> pure RatingDoesntExist
        Just rating ->
          pure $ RatingExists MealSynopsis
            { mealSynopsisTitle = title
            , mealSynopsisPermalink = permalink
            , mealSynopsisHeading = heading
            , mealSynopsisImages = images
            , mealSynopsisRating = rating
            , mealSynopsisOrders = orders
            , mealSynopsisTags = catMaybes $ tagExistsToMaybe <$> tags
            , mealSynopsisDiets = diets
            , mealSynopsisPrice = price
            }


-- | Witness a ChefSynopsis data-view
getChefSynopsis :: StoredChefId
                -> SystemM
                   ( ChefExists
                     ( ReviewExists ChefSynopsis))
getChefSynopsis chefId = do
  SystemEnv{systemEnvReviews} <- getSystemEnv

  mStoredChef <- liftDb $ do
    mChef <- get chefId
    case mChef of
      Nothing -> pure ChefDoesntExist
      Just (StoredChef _ name permalink _ _ avatar) -> do
        mTags <- getChefTags chefId
        case mTags of
          ChefDoesntExist -> pure ChefDoesntExist
          ChefExists tags -> fmap ChefExists $ do
            orders <- count [StoredOrderChef ==. chefId]
            pure (name,permalink,avatar,tags,orders)
  case mStoredChef of
    ChefDoesntExist -> pure ChefDoesntExist
    ChefExists (name,permalink,avatar,tags,orders) -> fmap ChefExists $ do
      mReviews <- liftIO (lookupChefReviews systemEnvReviews chefId)
      case mReviews of
        Nothing -> pure ReviewDoesntExist
        Just (rating,_) ->
          pure $ ReviewExists ChefSynopsis
            { chefSynopsisName = name
            , chefSynopsisPermalink = permalink
            , chefSynopsisImage = avatar
            , chefSynopsisRating = rating
            , chefSynopsisOrders = orders
            , chefSynopsisTags = catMaybes $ tagExistsToMaybe <$> tags
            }


-- | Witness all MenuSynopsis data-views belonging to a chef
getChefMenuSynopses :: StoredChefId
                    -> ReaderT SqlBackend IO
                       (ChefExists [MenuExists MenuSynopsis])
getChefMenuSynopses chefId = do
  mChef <- get chefId
  case mChef of
    Nothing -> pure ChefDoesntExist
    Just _ -> fmap ChefExists $ do
      xs <- selectList [StoredMenuAuthor ==. chefId] []
      fmap catMaybes $ forM xs $ \(Entity menuId (StoredMenu published deadline heading _ images _)) ->
        case published of
          Nothing -> pure Nothing
          Just p -> fmap Just $ do
            mTags <- getMenuTags menuId
            case mTags of
              MenuDoesntExist -> pure MenuDoesntExist
              MenuExists tags ->
                pure $ MenuExists MenuSynopsis
                  { menuSynopsisPublished = p
                  , menuSynopsisDeadline = deadline
                  , menuSynopsisHeading = heading
                  , menuSynopsisTags = catMaybes $ tagExistsToMaybe <$> tags
                  , menuSynopsisImages = images
                  }


-- | Witness all MealSynopsis data-views belonging to a Menu
getMenuMealSynopses :: StoredMenuId
                    -> SystemM (MenuExists [MealExists (RatingExists MealSynopsis)])
getMenuMealSynopses menuId = do
  mXs <- liftDb $ do
    mMenu <- get menuId
    case mMenu of
      Nothing -> pure MenuDoesntExist
      Just _ -> MenuExists <$> selectList [StoredMealMenu ==. menuId] []
  case mXs of
    MenuDoesntExist -> pure MenuDoesntExist
    MenuExists xs -> fmap MenuExists $ forM xs $ \(Entity mealId _) ->
      getMealSynopsis mealId


-- | Witness a Chef data-view
browseChef :: Permalink
           -> SystemM
              ( ChefUnique
                ( ChefExists
                  ( ReviewExists Chef)))
browseChef chefPermalink = do
  SystemEnv{systemEnvReviews} <- getSystemEnv

  liftDb $ do
    mChefEnt <- getBy (UniqueChefPermalink chefPermalink)
    case mChefEnt of
      Nothing -> pure ChefNotUnique
      Just (Entity chefId (StoredChef _ name permalink bio images _)) -> fmap ChefUnique $ do
        mTags <- getChefTags chefId
        case mTags of
          ChefDoesntExist -> pure ChefDoesntExist
          ChefExists tags -> do
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
              ChefDoesntExist -> pure ChefDoesntExist
              ChefExists meals -> fmap ChefExists $ do
                mReviews <- liftIO (lookupChefReviews systemEnvReviews chefId)
                case mReviews of
                  Nothing -> pure ReviewDoesntExist
                  Just (rating,reviews) ->
                    pure $ ReviewExists Chef
                      { chefName = name
                      , chefPermalink = permalink
                      , chefImages = images
                      , chefBio = bio
                      , chefRating = rating
                      , chefReviews = getReviewSynopsis <$> reviews
                      , chefActiveOrders = activeOrders
                      , chefTotalOrders = totalOrders
                      , chefTags = catMaybes $ tagExistsToMaybe <$> tags
                      , chefMenus = catMaybes $ menuExistsToMaybe <$> meals
                      }


-- | Witness a Menu data-view
browseMenu :: Permalink
           -> Day
           -> SystemM
              ( ChefUnique
                ( MenuUnique
                  ( MenuPublished
                    ( ChefExists
                      ( ReviewExists
                        ( MenuExists Menu))))))
browseMenu chefPermalink deadline = do
  mMenuDeets <- liftDb $ do
    mChef <- getBy (UniqueChefPermalink chefPermalink)
    case mChef of
      Nothing -> pure ChefNotUnique
      Just (Entity chefId _) -> fmap ChefUnique $ do
        mMenu <- getBy (UniqueMenuDeadline chefId deadline)
        case mMenu of
          Nothing -> pure MenuNotUnique
          Just (Entity menuId (StoredMenu mPub _ _ desc _ _)) -> fmap MenuUnique $
            case mPub of
              Nothing -> pure MenuNotPublished
              Just published -> pure $ MenuPublished (menuId,published,desc,chefId)

  case mMenuDeets of
    ChefNotUnique -> pure ChefNotUnique
    ChefUnique MenuNotUnique -> pure (ChefUnique MenuNotUnique)
    ChefUnique (MenuUnique MenuNotPublished) -> pure $ ChefUnique $ MenuUnique MenuNotPublished
    ChefUnique (MenuUnique (MenuPublished (menuId,published,desc,chefId))) -> fmap (ChefUnique . MenuUnique . MenuPublished) $ do
      mChef <- getChefSynopsis chefId
      case mChef of
        ChefDoesntExist -> pure ChefDoesntExist
        ChefExists ReviewDoesntExist -> pure (ChefExists ReviewDoesntExist)
        ChefExists (ReviewExists chef) -> fmap (ChefExists . ReviewExists) $ do
          mMeals <- getMenuMealSynopses menuId
          case mMeals of
            MenuDoesntExist -> pure MenuDoesntExist
            MenuExists meals ->
              pure $ MenuExists Menu
                { menuPublished = published
                , menuDeadline = deadline
                , menuDescription = desc
                , menuAuthor = chef
                , menuMeals = catMaybes $ (ratingExistsToMaybe <=< mealExistsToMaybe) <$> meals
                }


-- | Witness a Meal data-view
browseMeal :: Permalink
           -> Day
           -> Permalink
           -> SystemM
              ( ChefUnique
                ( MenuUnique
                  ( MealUnique
                    ( MealExists
                      ( RatingExists Meal)))))
browseMeal chefPermalink deadline mealPermalink = do
  SystemEnv{systemEnvReviews} <- getSystemEnv

  liftDb $ do
    mChef <- getBy (UniqueChefPermalink chefPermalink)
    case mChef of
      Nothing -> pure ChefNotUnique
      Just (Entity chefId _) -> fmap ChefUnique $ do
        mMenu <- getBy (UniqueMenuDeadline chefId deadline)
        case mMenu of
          Nothing -> pure MenuNotUnique
          Just (Entity menuId _) -> fmap MenuUnique $ do
            mMeal <- getBy (UniqueMealPermalink menuId mealPermalink)
            case mMeal of
              Nothing -> pure MealNotUnique
              Just (Entity mealId (StoredMeal title permalink _ _ desc inst images price)) -> fmap MealUnique $ do
                mIngDiets <- getMealIngredientsDiets mealId
                case mIngDiets of
                  MealDoesntExist -> pure MealDoesntExist
                  MealExists (ings,diets) -> do
                    mTags <- getMealTags mealId
                    case mTags of
                      MealDoesntExist -> pure MealDoesntExist
                      MealExists tags -> fmap MealExists $ do
                        orders <- count
                          [ StoredOrderMeal ==. mealId
                          , StoredOrderProgress !=. DeliveredProgress
                          ]
                        reviewIds <-
                          fmap (fmap (\(Entity reviewId _) -> reviewId)) $
                            selectList [StoredReviewMeal ==. mealId] []
                        mRating <- liftIO (lookupMealRating systemEnvReviews mealId)
                        case mRating of
                          Nothing -> pure RatingDoesntExist
                          Just rating -> do
                            reviews <- do
                              xs <- forM reviewIds getReview
                              pure $ catMaybes $ reviewExistsToMaybe <$> xs
                            ings' <- fmap catMaybes $ forM ings getIngredientByName
                            pure $ RatingExists Meal
                              { mealTitle = title
                              , mealPermalink = permalink
                              , mealDescription = desc
                              , mealInstructions = inst
                              , mealImages = images
                              , mealIngredients = ings'
                              , mealDiets = diets
                              , mealTags = catMaybes $ tagExistsToMaybe <$> tags
                              , mealOrders = orders
                              , mealRating = rating
                              , mealReviews = getReviewSynopsis <$> reviews
                              , mealPrice = price
                              }


-- | Witness all CartEntry data-views belonging to a login session's customer
getCart :: AuthToken
        -> SystemM
           ( UserExists [CartEntry])
getCart authToken = do
  mUserId <- getUserId authToken
  case mUserId of
    UserDoesntExist -> pure UserDoesntExist
    UserExists userId -> liftDb $
      fmap
        (UserExists . fmap (\(Entity _ (CartRelation _ mealId vol time)) ->
          CartEntry mealId vol time))
        $ selectList [CartRelationCustomer ==. userId] []


-- | Add a meal to a customer's cart
addToCart :: AuthToken
          -> Permalink
          -> Day
          -> Permalink
          -> Int
          -> SystemM
             ( UserExists
               ( MealExists JSONUnit))
addToCart authToken chefPermalink deadline mealPermalink vol = do
  mUserId <- getUserId authToken
  case mUserId of
    UserDoesntExist -> pure UserDoesntExist
    UserExists userId -> fmap UserExists $ liftDb $ do
      mMealId <- getMealId chefPermalink deadline mealPermalink
      case mMealId of
        Nothing -> pure MealDoesntExist
        Just mealId -> fmap MealExists $ do
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
          pure JSONUnit

-- checkout :: ?

-- | Witness all Order data-views belonging to a login-session's customer
getOrders :: AuthToken
          -> SystemM
             ( UserExists
               ( CustomerUnique [MealExists (RatingExists Order)]))
getOrders authToken = do
  mUserId <- getUserId authToken
  case mUserId of
    UserDoesntExist -> pure UserDoesntExist
    UserExists userId -> fmap UserExists $ do
      mXs <- liftDb $ do
        mCust <- getBy (UniqueCustomer userId)
        case mCust of
          Nothing -> pure CustomerNotUnique
          Just (Entity custId _) ->
            CustomerUnique <$> selectList [StoredOrderCustomer ==. custId] []
      case mXs of
        CustomerNotUnique -> pure CustomerNotUnique
        CustomerUnique xs -> fmap CustomerUnique $
          forM xs $ \(Entity _ (StoredOrder _ mealId _ _ vol progress time)) -> do
            mMealSynopsis <- getMealSynopsis mealId
            case mMealSynopsis of
              MealDoesntExist -> pure MealDoesntExist
              MealExists RatingDoesntExist -> pure (MealExists RatingDoesntExist)
              MealExists (RatingExists meal) ->
                pure $ MealExists $ RatingExists Order
                  { orderMeal = meal
                  , orderProgress = progress
                  , orderTime = time
                  , orderVolume = vol
                  }
