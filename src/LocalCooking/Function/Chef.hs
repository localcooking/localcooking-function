{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  , DeriveGeneric
  , OverloadedStrings
  #-}

module LocalCooking.Function.Chef
  ( ValidateChefError (..), validateChef
  , getChef, setChef, unsafeStoreChef
  , getMenus, newMenu, setMenu, unsafeStoreNewMenu, unsafeStoreSetMenu
  , getMeals, newMeal, setMeal, unsafeStoreNewMeal, unsafeStoreSetMeal
  ) where

import LocalCooking.Function.Semantics
  ( getChefTags, getMealTags, getMenuTags, getMealIngredientsDiets
  , assignChefTags, assignMealTags, assignMenuTags, assignMealIngredients
  )
import LocalCooking.Function.System
  (SystemM, SystemEnv (..), getUserId, getSystemEnv)
import LocalCooking.Semantics.Chef
  ( SetChef (..), ChefValid (..), MenuSettings (..), MealSettings (..)
  )
import LocalCooking.Semantics.ContentRecord
  ( ContentRecord (ChefRecord, ProfileRecord)
  , ChefRecord (..), ProfileRecord (ProfileRecordChef)
  , contentRecordVariant
  )
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.User.Role (UserRole (Chef))
import LocalCooking.Database.Schema
  ( hasRole, StoredChef (..), StoredMenu (..), StoredMeal (..)
  , StoredChefId, StoredMenuId, StoredMealId, StoredUserId
  , MenuTagRelation (..), ChefTagRelation (..), MealTagRelation (..)
  , MealIngredient (..)
  , EntityField
    ( StoredChefName, StoredChefPermalink
    , StoredChefImages, StoredChefAvatar
    , StoredChefBio
    , StoredMenuAuthor, StoredMenuPublished
    , StoredMenuDeadline, StoredMenuDescription
    , StoredMenuHeading, StoredMenuImages
    , StoredMealMenu, StoredMealDescription
    , StoredMealHeading, StoredMealImages
    , StoredMealInstructions, StoredMealPermalink
    , StoredMealPrice, StoredMealTitle
    )
  , Unique
    ( UniqueChefOwner, UniqueMealPermalink, UniqueMenuDeadline
    , UniqueStoredMealTag, UniqueStoredChefTag, UniqueChefPermalink
    )
  , getStoredIngredientTagId
  )
import LocalCooking.Database.Schema.Content
  ( StoredRecordSubmission (..)
  )

import Data.Maybe (catMaybes)
import Data.Time (getCurrentTime)
import Data.Aeson.JSONTuple (JSONTuple (..))
import Control.Monad (forM_, forM)
import Control.Monad.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.Persist (Entity (..), (==.), (=.))
import Database.Persist.Sql (SqlBackend, runSqlPool)
import Database.Persist.Class (selectList, getBy, insert, insert_, update, get)



data ValidateChefError
  = ChefInvalidNoName
  | ChefInvalidNoPermalink
  | ChefInvalidNoAvatar
  | ChefInvalidNotUniquePermalink


-- | Turns user-supplied SetChef partial structure into a valid one, checking
--   for uniqueness.
validateChef :: SetChef -> ReaderT SqlBackend IO (Either ValidateChefError ChefValid)
validateChef SetChef{..} =
  case setChefName of
    Nothing -> pure (Left ChefInvalidNoName)
    Just name -> case setChefPermalink of
      Nothing -> pure (Left ChefInvalidNoPermalink)
      Just permalink -> case setChefAvatar of
        Nothing -> pure (Left ChefInvalidNoAvatar)
        Just avatar -> do
          mExisting <- getBy (UniqueChefPermalink permalink)
          case mExisting of
            Just _ -> pure (Left ChefInvalidNotUniquePermalink)
            Nothing -> pure $ Right ChefValid
              { chefValidName = name
              , chefValidPermalink = permalink
              , chefValidImages = setChefImages
              , chefValidAvatar = avatar
              , chefValidBio = setChefBio
              , chefValidTags = setChefTags
              }


-- | Witness the stored chef profile of a logged-in user - TODO what about
--   universally? From a permalink?
--   Context: chef.localcooking.com - a chef observing another chef's profile.
getChef :: AuthToken -> SystemM (Maybe ChefValid)
getChef authToken = do
  mChef <- getChefFromAuthToken authToken
  case mChef of
    Nothing -> pure Nothing
    Just (JSONTuple chefId (StoredChef _ name permalink bio images avatar)) -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        mTags <- getChefTags chefId
        case mTags of
          Nothing -> pure Nothing
          Just tags ->
            pure $ Just ChefValid
              { chefValidName = name
              , chefValidPermalink = permalink
              , chefValidImages = images
              , chefValidAvatar = avatar
              , chefValidBio = bio
              , chefValidTags = tags
              }


-- | Pysically store the content of a 'ChefValid' data-view into the database
unsafeStoreChef :: StoredUserId -> ChefValid -> SystemM (Maybe StoredChefId)
unsafeStoreChef userId ChefValid{..} = do
  mChef <- getChefFromUserId userId
  SystemEnv{systemEnvDatabase} <- getSystemEnv
  liftIO $ flip runSqlPool systemEnvDatabase $ case mChef of
    Nothing -> do
      chefId <- insert $ StoredChef
        userId
        chefValidName
        chefValidPermalink
        chefValidBio
        chefValidImages
        chefValidAvatar
      forM_ chefValidTags $ \t -> do
        mChefTagId <- getBy (UniqueStoredChefTag t)
        case mChefTagId of
          Nothing -> pure ()
          Just (Entity chefTagId _) -> insert_ (ChefTagRelation chefId chefTagId)
      pure (Just chefId)
    Just (JSONTuple chefId _) -> do
      update chefId
        [ StoredChefName =. chefValidName
        , StoredChefPermalink =. chefValidPermalink
        , StoredChefBio =. chefValidBio
        , StoredChefImages =. chefValidImages
        , StoredChefAvatar =. chefValidAvatar
        ]
      assignChefTags chefId chefValidTags
      pure (Just chefId)


-- | Marshall a user-supplied chef profile into the content submission system
setChef :: AuthToken -> SetChef -> SystemM Bool
setChef authToken setChef' = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure False
    Just userId -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        now <- liftIO getCurrentTime
        let record = ProfileRecord (ProfileRecordChef setChef')
        insert_ $ StoredRecordSubmission userId now record (contentRecordVariant record)
        pure True


-- | Lookup all of a chef's own menus
getMenus :: AuthToken -> SystemM (Maybe [JSONTuple StoredMenuId MenuSettings])
getMenus authToken = do
  mChef <- getChefFromAuthToken authToken
  case mChef of
    Nothing -> pure Nothing
    Just (JSONTuple chefId _) -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        xs <- selectList [StoredMenuAuthor ==. chefId] []
        fmap (Just . catMaybes)
          $ forM xs $ \(Entity menuId (StoredMenu pub dead heading desc imgs _)) -> do
              mTags <- getMenuTags menuId
              case mTags of
                Nothing -> pure Nothing
                Just tags ->
                  pure $ Just $ JSONTuple
                    menuId
                    MenuSettings
                    { menuSettingsPublished = pub
                    , menuSettingsDeadline = dead
                    , menuSettingsHeading = heading
                    , menuSettingsDescription = desc
                    , menuSettingsImages = imgs
                    , menuSettingsTags = tags
                    }


-- | Physically store a chef's new menu.
unsafeStoreNewMenu :: StoredUserId -> MenuSettings -> SystemM (Maybe StoredMenuId)
unsafeStoreNewMenu userId MenuSettings{..} = do
  mChef <- getChefFromUserId userId
  case mChef of
    Nothing -> pure Nothing
    Just (JSONTuple chefId _) -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        mEnt <- getBy (UniqueMenuDeadline chefId menuSettingsDeadline)
        case mEnt of
          Just _ -> pure Nothing
          Nothing -> do
            menuId <- insert $ StoredMenu
              menuSettingsPublished
              menuSettingsDeadline
              menuSettingsHeading
              menuSettingsDescription
              menuSettingsImages
              chefId
            forM_ menuSettingsTags $ \tag -> do
              mTagId <- getBy (UniqueStoredMealTag tag)
              case mTagId of
                Nothing -> pure ()
                Just (Entity tagId _) -> insert_ (MenuTagRelation menuId tagId)
            pure (Just menuId)


-- | Marhsall a new user-supplied chef menu into the content submission system
newMenu :: AuthToken -> MenuSettings -> SystemM Bool
newMenu authToken menu = do
  mChef <- getChefFromAuthToken authToken
  case mChef of
    Nothing -> pure False
    Just _ -> do
      mUserId <- getUserId authToken
      case mUserId of
        Nothing -> pure False
        Just userId -> do
          SystemEnv{systemEnvDatabase} <- getSystemEnv
          liftIO $ flip runSqlPool systemEnvDatabase $ do
            now <- liftIO getCurrentTime
            let record = ChefRecord (ChefRecordNewMenu menu)
            insert_ $ StoredRecordSubmission userId now record (contentRecordVariant record)
            pure True


-- | Physically store a chef's adjusted menu into the database.
unsafeStoreSetMenu :: StoredUserId -> JSONTuple StoredMenuId MenuSettings -> SystemM Bool
unsafeStoreSetMenu userId (JSONTuple menuId MenuSettings{..}) = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv
  liftIO $ flip runSqlPool systemEnvDatabase $ do
    isAuthorized <- hasRole userId Chef
    if not isAuthorized
      then pure False
      else do
        mEnt <- get menuId
        case mEnt of
          Nothing -> pure False
          _ -> do
            update menuId
              [ StoredMenuPublished =. menuSettingsPublished
              , StoredMenuDeadline =. menuSettingsDeadline
              , StoredMenuHeading =. menuSettingsHeading
              , StoredMenuDescription =. menuSettingsDescription
              , StoredMenuImages =. menuSettingsImages
              ]
            assignMenuTags menuId menuSettingsTags
            pure True


-- | Marhsall a user-supplied adjustment to a chef's menu into the content submission system
setMenu :: AuthToken -> JSONTuple StoredMenuId MenuSettings -> SystemM Bool
setMenu authToken menu = do
  isAuthorized <- verifyChefhood authToken
  if not isAuthorized
    then pure False
    else do
      mUserId <- getUserId authToken
      case mUserId of
        Nothing -> pure False
        Just userId -> do
          SystemEnv{systemEnvDatabase} <- getSystemEnv
          liftIO $ flip runSqlPool systemEnvDatabase $ do
            now <- liftIO getCurrentTime
            let record = ChefRecord (ChefRecordSetMenu menu)
            insert_ $ StoredRecordSubmission userId now record (contentRecordVariant record)
            pure True


-- | Lookup all of the meals in a chef's own menu
getMeals :: AuthToken -> StoredMenuId -> SystemM (Maybe [JSONTuple StoredMealId MealSettings])
getMeals authToken menuId = do
  isAuthorized <- verifyChefhood authToken
  if not isAuthorized
    then pure Nothing
    else do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        xs <- selectList [StoredMealMenu ==. menuId] []
        fmap (Just . catMaybes) $
          forM xs $ \(Entity mealId (StoredMeal title permalink _ heading desc inst imgs price)) -> do
            mTags <- getMealTags mealId
            case mTags of
              Nothing -> pure Nothing
              Just tags -> do
                mIngDiets <- getMealIngredientsDiets mealId
                case mIngDiets of
                  Nothing -> pure Nothing
                  Just (ings,_) ->
                    pure $ Just $ JSONTuple
                      mealId
                      MealSettings
                      { mealSettingsTitle = title
                      , mealSettingsPermalink = permalink
                      , mealSettingsHeading = heading
                      , mealSettingsDescription = desc
                      , mealSettingsInstructions = inst
                      , mealSettingsImages = imgs
                      , mealSettingsIngredients = ings
                      , mealSettingsTags = tags
                      , mealSettingsPrice = price
                      }


-- | Physically store a chef's new meal into the database.
unsafeStoreNewMeal :: StoredUserId -> JSONTuple StoredMenuId MealSettings -> SystemM (Maybe StoredMealId)
unsafeStoreNewMeal userId (JSONTuple menuId MealSettings{..}) = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv
  liftIO $ flip runSqlPool systemEnvDatabase $ do
    isAuthorized <- hasRole userId Chef
    if not isAuthorized
      then pure Nothing
      else do
        mEnt <- getBy (UniqueMealPermalink menuId mealSettingsPermalink)
        case mEnt of
          Just _ -> pure Nothing
          Nothing -> do
            mealId <- insert $ StoredMeal
              mealSettingsTitle
              mealSettingsPermalink
              menuId
              mealSettingsHeading
              mealSettingsDescription
              mealSettingsInstructions
              mealSettingsImages
              mealSettingsPrice
            forM_ mealSettingsIngredients $ \ingName -> do
              mIngId <- getStoredIngredientTagId ingName
              case mIngId of
                Nothing -> pure ()
                Just ingId -> insert_ (MealIngredient mealId ingId)
            forM_ mealSettingsTags $ \tag -> do
              mTagId <- getBy (UniqueStoredMealTag tag)
              case mTagId of
                Nothing -> pure ()
                Just (Entity tagId _) -> insert_ (MealTagRelation mealId tagId)
            pure (Just mealId)


-- | Marhsall a user-supplied chef's new meal into the content submission system
newMeal :: AuthToken -> JSONTuple StoredMenuId MealSettings -> SystemM Bool
newMeal authToken meal = do
  isAuthorized <- verifyChefhood authToken
  if not isAuthorized
    then pure False
    else do
      mUserId <- getUserId authToken
      case mUserId of
        Nothing -> pure False
        Just userId -> do
          SystemEnv{systemEnvDatabase} <- getSystemEnv
          liftIO $ flip runSqlPool systemEnvDatabase $ do
            now <- liftIO getCurrentTime
            let record = ChefRecord (ChefRecordNewMeal meal)
            insert_ $ StoredRecordSubmission userId now record (contentRecordVariant record)
            pure True


-- | Physically store a chef's adjusted meal into the database.
unsafeStoreSetMeal :: StoredUserId -> JSONTuple StoredMenuId (JSONTuple StoredMealId MealSettings) -> SystemM Bool
unsafeStoreSetMeal userId (JSONTuple menuId (JSONTuple mealId MealSettings{..})) = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv
  liftIO $ flip runSqlPool systemEnvDatabase $ do
    isAuthorized <- hasRole userId Chef
    if not isAuthorized
      then pure False
      else do
        mEnt <- get mealId
        case mEnt of
          Nothing -> pure False
          _ -> do
            update mealId
              [ StoredMealTitle =. mealSettingsTitle
              , StoredMealPermalink =. mealSettingsPermalink
              , StoredMealMenu =. menuId
              , StoredMealHeading =. mealSettingsHeading
              , StoredMealDescription =. mealSettingsDescription
              , StoredMealInstructions =. mealSettingsInstructions
              , StoredMealImages =. mealSettingsImages
              , StoredMealPrice =. mealSettingsPrice
              ]
            assignMealTags mealId mealSettingsTags
            assignMealIngredients mealId mealSettingsIngredients
            pure True


-- | Marhsall a user-supplied chef's adjusted meal into the content submission system
setMeal :: AuthToken -> JSONTuple StoredMenuId (JSONTuple StoredMealId MealSettings) -> SystemM Bool
setMeal authToken meal = do
  isAuthorized <- verifyChefhood authToken
  if not isAuthorized
    then pure False
    else do
      mUserId <- getUserId authToken
      case mUserId of
        Nothing -> pure False
        Just userId -> do
          SystemEnv{systemEnvDatabase} <- getSystemEnv
          liftIO $ flip runSqlPool systemEnvDatabase $ do
            now <- liftIO getCurrentTime
            let record = ChefRecord (ChefRecordSetMeal meal)
            insert_ $ StoredRecordSubmission userId now record (contentRecordVariant record)
            pure True





-- * Utilities

-- | Verify that a login session belongs to an authentic Chef according to
--   the user's Role
verifyChefhood :: AuthToken -> SystemM Bool
verifyChefhood authToken = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure False
    Just userId -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ hasRole userId Chef

-- | Obtain a chef's profile identified by a login session
getChefFromAuthToken :: AuthToken -> SystemM (Maybe (JSONTuple StoredChefId StoredChef))
getChefFromAuthToken authToken = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure Nothing
    Just userId -> getChefFromUserId userId

-- | Obtain a chef's profile identified by a user id
getChefFromUserId :: StoredUserId -> SystemM (Maybe (JSONTuple StoredChefId StoredChef))
getChefFromUserId userId = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv
  liftIO $ flip runSqlPool systemEnvDatabase $ do
    isChef <- hasRole userId Chef
    if not isChef
      then pure Nothing
      else do
        mEnt <- getBy (UniqueChefOwner userId)
        case mEnt of
          Nothing -> pure Nothing
          Just (Entity chefId chef) -> pure $ Just $ JSONTuple chefId chef
