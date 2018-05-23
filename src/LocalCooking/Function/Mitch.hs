{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  #-}

module LocalCooking.Function.Mitch where

import LocalCooking.Semantics.Mitch (Customer (..))
import LocalCooking.Function.System (AppM, SystemEnv (..), TokenContexts (..))
import LocalCooking.Function.System.AccessToken (lookupAccess)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Database.Query.IngredientDiet (getDietId, getStoredIngredientId)
import LocalCooking.Database.Schema.User.Customer (StoredDietPreference (..), EntityField (StoredDietPreferenceDietPreferenceOwner, StoredDietPreferenceDietPreferenceDiet, StoredCustomerStoredCustomerAddress, StoredCustomerStoredCustomerName, StoredAllergyAllergy, StoredAllergyAllergyOwner), StoredCustomer (..), StoredAllergy (..), Unique (UniqueCustomer))

import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import Database.Persist (Entity (..), (==.), (=.))
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Class (selectList, getBy, insert, insert_, deleteWhere, update)


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