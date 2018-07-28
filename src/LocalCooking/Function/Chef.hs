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
  (SystemM, SystemEnv (..), getSystemEnv, liftDb)
import LocalCooking.Function.User (getUserId, verifyRole)
import LocalCooking.Semantics.Chef
  ( SetChef (..), ChefValid (..), MenuSettings (..), MealSettings (..)
  , ChefUnique (..), ChefExists (..)
  )
import LocalCooking.Semantics.Mitch
  (MealUnique (..), MealExists (..), MenuExists (..), MenuUnique (..))
import LocalCooking.Semantics.User (UserExists (..), HasRole (..))
import LocalCooking.Semantics.Tag (tagExistsToMaybe)
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
import Data.Aeson.JSONUnit (JSONUnit (..))
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
getChef :: AuthToken
        -> SystemM
           ( UserExists
             ( HasRole
               ( ChefUnique
                 ( ChefExists ChefValid))))
getChef authToken = do
  mChef <- getChefFromAuthToken authToken
  case mChef of
    UserDoesntExist -> pure UserDoesntExist
    UserExists DoesntHaveRole -> pure (UserExists DoesntHaveRole)
    UserExists (HasRole ChefNotUnique) -> pure $ UserExists $ HasRole ChefNotUnique
    UserExists (HasRole (ChefUnique
      (JSONTuple chefId (StoredChef _ name permalink bio images avatar)))) -> do
      fmap (UserExists . HasRole . ChefUnique) $ liftDb $ do
        mTags <- getChefTags chefId
        case mTags of
          ChefDoesntExist -> pure ChefDoesntExist
          ChefExists tags ->
            pure $ ChefExists ChefValid
              { chefValidName = name
              , chefValidPermalink = permalink
              , chefValidImages = images
              , chefValidAvatar = avatar
              , chefValidBio = bio
              , chefValidTags = catMaybes $ tagExistsToMaybe <$> tags
              }


-- | Pysically store the content of a 'ChefValid' data-view into the database
unsafeStoreChef :: StoredUserId
                -> ChefValid
                -> SystemM
                   ( HasRole
                     ( ChefUnique StoredChefId))
unsafeStoreChef userId ChefValid{..} = do
  mChef <- getChefFromUserId userId
  case mChef of
    DoesntHaveRole -> pure DoesntHaveRole
    HasRole mChef' -> fmap (HasRole . ChefUnique) $ liftDb $ case mChef' of
      ChefNotUnique -> do
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
        pure chefId
      ChefUnique (JSONTuple chefId _) -> do
        update chefId
          [ StoredChefName =. chefValidName
          , StoredChefPermalink =. chefValidPermalink
          , StoredChefBio =. chefValidBio
          , StoredChefImages =. chefValidImages
          , StoredChefAvatar =. chefValidAvatar
          ]
        assignChefTags chefId chefValidTags
        pure chefId


-- | Marshall a user-supplied chef profile into the content submission system
setChef :: AuthToken
        -> SetChef
        -> SystemM
           ( UserExists (HasRole JSONUnit))
setChef authToken setChef' =
  verifyRole Chef authToken $ \userId -> liftDb $ do
    now <- liftIO getCurrentTime
    let record = ProfileRecord (ProfileRecordChef setChef')
    insert_ $ StoredRecordSubmission userId now record (contentRecordVariant record)
    pure JSONUnit


-- | Lookup all of a chef's own menus
getMenus :: AuthToken
         -> SystemM
            ( UserExists
              ( HasRole
                ( ChefUnique
                  [ MenuExists (JSONTuple StoredMenuId MenuSettings)])))
getMenus authToken = do
  mChef <- getChefFromAuthToken authToken
  case mChef of
    UserDoesntExist -> pure UserDoesntExist
    UserExists DoesntHaveRole -> pure (UserExists DoesntHaveRole)
    UserExists (HasRole ChefNotUnique) -> pure $ UserExists $ HasRole ChefNotUnique
    UserExists (HasRole (ChefUnique (JSONTuple chefId _))) ->
      fmap (UserExists . HasRole . ChefUnique) $ liftDb $ do
        xs <- selectList [StoredMenuAuthor ==. chefId] []
        forM xs $ \(Entity menuId (StoredMenu pub dead heading desc imgs _)) -> do
          mTags <- getMenuTags menuId
          case mTags of
            MenuDoesntExist -> pure MenuDoesntExist
            MenuExists tags ->
              pure $ MenuExists $ JSONTuple
                menuId
                MenuSettings
                { menuSettingsPublished = pub
                , menuSettingsDeadline = dead
                , menuSettingsHeading = heading
                , menuSettingsDescription = desc
                , menuSettingsImages = imgs
                , menuSettingsTags = catMaybes $ tagExistsToMaybe <$> tags
                }


-- | Physically store a chef's new menu.
unsafeStoreNewMenu :: StoredUserId
                   -> MenuSettings
                   -> SystemM
                      ( HasRole
                        ( ChefUnique
                          ( MenuUnique StoredMenuId)))
unsafeStoreNewMenu userId MenuSettings{..} = do
  mChef <- getChefFromUserId userId
  case mChef of
    DoesntHaveRole -> pure DoesntHaveRole
    HasRole ChefNotUnique -> pure (HasRole ChefNotUnique)
    HasRole (ChefUnique (JSONTuple chefId _)) -> fmap (HasRole . ChefUnique) $ liftDb $ do
      mEnt <- getBy (UniqueMenuDeadline chefId menuSettingsDeadline)
      case mEnt of
        Just _ -> pure MenuNotUnique
        Nothing -> fmap MenuUnique $ do
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
          pure menuId


-- | Marhsall a new user-supplied chef menu into the content submission system
newMenu :: AuthToken
        -> MenuSettings
        -> SystemM (UserExists (HasRole JSONUnit))
newMenu authToken menu = do
  verifyRole Chef authToken $ \userId -> liftDb $ do
    now <- liftIO getCurrentTime
    let record = ChefRecord (ChefRecordNewMenu menu)
    insert_ $ StoredRecordSubmission userId now record (contentRecordVariant record)
    pure JSONUnit


-- | Physically store a chef's adjusted menu into the database.
unsafeStoreSetMenu :: StoredUserId
                   -> JSONTuple StoredMenuId MenuSettings
                   -> SystemM
                      ( HasRole (MenuExists JSONUnit))
unsafeStoreSetMenu userId (JSONTuple menuId MenuSettings{..}) = liftDb $ do
  isAuthorized <- hasRole userId Chef
  if not isAuthorized
    then pure DoesntHaveRole
    else fmap HasRole $ do
      mEnt <- get menuId
      case mEnt of
        Nothing -> pure MenuDoesntExist
        _ -> fmap MenuExists $ do
          update menuId
            [ StoredMenuPublished =. menuSettingsPublished
            , StoredMenuDeadline =. menuSettingsDeadline
            , StoredMenuHeading =. menuSettingsHeading
            , StoredMenuDescription =. menuSettingsDescription
            , StoredMenuImages =. menuSettingsImages
            ]
          assignMenuTags menuId menuSettingsTags
          pure JSONUnit


-- | Marhsall a user-supplied adjustment to a chef's menu into the content submission system
setMenu :: AuthToken
        -> JSONTuple StoredMenuId MenuSettings
        -> SystemM
           (UserExists (HasRole JSONUnit))
setMenu authToken menu =
  verifyRole Chef authToken $ \userId -> liftDb $ do
    now <- liftIO getCurrentTime
    let record = ChefRecord (ChefRecordSetMenu menu)
    insert_ $ StoredRecordSubmission userId now record (contentRecordVariant record)
    pure JSONUnit


-- | Lookup all of the meals in a chef's own menu
getMeals :: AuthToken
         -> StoredMenuId
         -> SystemM
            ( UserExists
              ( HasRole [MealExists (JSONTuple StoredMealId MealSettings)]))
getMeals authToken menuId = do
  verifyRole Chef authToken $ \_ -> liftDb $ do
    xs <- selectList [StoredMealMenu ==. menuId] []
    forM xs $ \(Entity mealId (StoredMeal title permalink _ heading desc inst imgs price)) -> do
      mTags <- getMealTags mealId
      case mTags of
        MealDoesntExist -> pure MealDoesntExist
        MealExists tags -> do
          mIngDiets <- getMealIngredientsDiets mealId
          case mIngDiets of
            MealDoesntExist -> pure MealDoesntExist
            MealExists (ings,_) ->
              pure $ MealExists $ JSONTuple
                mealId
                MealSettings
                { mealSettingsTitle = title
                , mealSettingsPermalink = permalink
                , mealSettingsHeading = heading
                , mealSettingsDescription = desc
                , mealSettingsInstructions = inst
                , mealSettingsImages = imgs
                , mealSettingsIngredients = ings
                , mealSettingsTags = catMaybes $ tagExistsToMaybe <$> tags
                , mealSettingsPrice = price
                }


-- | Physically store a chef's new meal into the database.
unsafeStoreNewMeal :: StoredUserId
                   -> JSONTuple StoredMenuId MealSettings
                   -> SystemM (HasRole (MealUnique StoredMealId))
unsafeStoreNewMeal userId (JSONTuple menuId MealSettings{..}) = do
  liftDb $ do
    isAuthorized <- hasRole userId Chef
    if not isAuthorized
      then pure DoesntHaveRole
      else fmap HasRole $ do
        mEnt <- getBy (UniqueMealPermalink menuId mealSettingsPermalink)
        case mEnt of
          Just _ -> pure MealNotUnique
          Nothing -> fmap MealUnique $ do
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
            pure mealId


-- | Marhsall a user-supplied chef's new meal into the content submission system
newMeal :: AuthToken
        -> JSONTuple StoredMenuId MealSettings
        -> SystemM (UserExists (HasRole JSONUnit))
newMeal authToken meal =
  verifyRole Chef authToken $ \userId -> liftDb $ do
    now <- liftIO getCurrentTime
    let record = ChefRecord (ChefRecordNewMeal meal)
    insert_ $ StoredRecordSubmission userId now record (contentRecordVariant record)
    pure JSONUnit


-- | Physically store a chef's adjusted meal into the database.
unsafeStoreSetMeal :: StoredUserId
                   -> JSONTuple StoredMenuId (JSONTuple StoredMealId MealSettings)
                   -> SystemM (HasRole (MealExists JSONUnit))
unsafeStoreSetMeal userId (JSONTuple menuId (JSONTuple mealId MealSettings{..})) = do
  liftDb $ do
    isAuthorized <- hasRole userId Chef
    if not isAuthorized
      then pure DoesntHaveRole
      else fmap HasRole $ do
        mEnt <- get mealId
        case mEnt of
          Nothing -> pure MealDoesntExist
          _ -> fmap MealExists $ do
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
            pure JSONUnit


-- | Marhsall a user-supplied chef's adjusted meal into the content submission system
setMeal :: AuthToken
        -> JSONTuple StoredMenuId (JSONTuple StoredMealId MealSettings)
        -> SystemM (UserExists (HasRole JSONUnit))
setMeal authToken meal = do
  verifyRole Chef authToken $ \userId -> liftDb $ do
    now <- liftIO getCurrentTime
    let record = ChefRecord (ChefRecordSetMeal meal)
    insert_ $ StoredRecordSubmission userId now record (contentRecordVariant record)
    pure JSONUnit





-- * Utilities

-- | Obtain a chef's profile identified by a login session
getChefFromAuthToken :: AuthToken
                     -> SystemM
                        ( UserExists
                          ( HasRole
                            ( ChefUnique
                              ( JSONTuple StoredChefId StoredChef))))
getChefFromAuthToken authToken = do
  mUserId <- getUserId authToken
  case mUserId of
    UserDoesntExist -> pure UserDoesntExist
    UserExists userId -> UserExists <$> getChefFromUserId userId

-- | Obtain a chef's profile identified by a user id
getChefFromUserId :: StoredUserId
                  -> SystemM
                     ( HasRole
                       ( ChefUnique
                         ( JSONTuple StoredChefId StoredChef)))
getChefFromUserId userId = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv
  liftIO $ flip runSqlPool systemEnvDatabase $ do
    isChef <- hasRole userId Chef
    if not isChef
      then pure DoesntHaveRole
      else fmap HasRole $ do
        mEnt <- getBy (UniqueChefOwner userId)
        case mEnt of
          Nothing -> pure ChefNotUnique
          Just (Entity chefId chef) -> pure $ ChefUnique $ JSONTuple chefId chef
