{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  #-}

module LocalCooking.Function.Mitch where

import LocalCooking.Semantics.Mitch (Customer (..), Chef (..), MenuSynopsis (..), getReviewSynopsis)
import LocalCooking.Function.System (AppM, SystemEnv (..), TokenContexts (..))
import LocalCooking.Function.System.Review (lookupChefReviews)
import LocalCooking.Function.System.AccessToken (lookupAccess)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Database.Query.IngredientDiet (getDietId, getStoredIngredientId)
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
  ( StoredMenu (..), StoredChef (..), StoredChefId
  , MenuTagRelation (..), ChefTagRelation (..)
  , EntityField
    ( MenuTagRelationMenuTagMenu, StoredMenuStoredMenuAuthor
    , StoredOrderStoredOrderChef, StoredMenuStoredMenuDeadline
    , StoredOrderStoredOrderMenu, ChefTagRelationChefTagChef
    )
  , Unique (UniqueChefPermalink)
  )

import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Data.Text.Permalink (Permalink)
import Data.IORef (newIORef, readIORef, modifyIORef)
import Data.Time (getCurrentTime, utctDay)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import Database.Persist (Entity (..), (==.), (=.), (>=.))
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Class (selectList, getBy, insert, insert_, deleteWhere, update, count)


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



getChefMenuSynopses :: StoredChefId -> AppM [MenuSynopsis]
getChefMenuSynopses chefId = do
  SystemEnv{systemEnvDatabase} <- ask

  flip runSqlPool systemEnvDatabase $ do
    xs <- selectList [StoredMenuStoredMenuAuthor ==. chefId] []
    fmap catMaybes $ forM xs $ \(Entity k (StoredMenu published deadline heading _ images _)) -> do
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

browseMeal :: Permalink -> Day -> Permalink -> AppM (Maybe Meal)


addToCart :: Permalink -> Day -> Permalink -> Int -> AppM Bool

checkout :: ?

getOrders :: AuthToken -> AppM (Maybe [Order])
