{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  , DeriveGeneric
  , OverloadedStrings
  #-}

module LocalCooking.Function.Content where

import LocalCooking.Function.Tag
  ( unsafeStoreChefTag, unsafeStoreCultureTag, unsafeStoreDietTag
  , unsafeStoreFarmTag, unsafeStoreIngredientTag, unsafeStoreMealTag)
import LocalCooking.Function.Chef
  ( unsafeStoreChef, unsafeStoreNewMenu, unsafeStoreSetMenu
  , unsafeStoreNewMeal, unsafeStoreSetMeal, validateChef
  )
import LocalCooking.Function.Mitch
  ( unsafeStoreCustomer, validateCustomer
  )
import LocalCooking.Function.System (SystemM, SystemEnv (..), getSystemEnv, liftDb)
import LocalCooking.Function.User (getUserId, verifyRole)
import LocalCooking.Semantics.Content
  ( SetEditor (..), EditorValid (..), GetRecordSubmissionPolicy (..)
  , SubmissionPolicy (..), SubmissionExists (..), EditorExists (..))
import LocalCooking.Semantics.User (UserExists (..), HasRole (..))
import LocalCooking.Semantics.Content.Approval
  ( GetEditor (..), GetRecordSubmission (..))
import LocalCooking.Semantics.ContentRecord
  ( ContentRecord (..), TagRecord (..), ChefRecord (..), ProfileRecord (..)
  , contentRecordVariant)
import LocalCooking.Semantics.ContentRecord.Variant
  (ContentRecordVariant)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.User.Role (UserRole (Editor))
import LocalCooking.Database.Schema
  ( hasRole, StoredEditor (..), StoredEditorId, StoredUserId
  , EntityField
    ( StoredEditorName
    )
  , Unique
    ( UniqueEditor
    )
  )
import LocalCooking.Database.Schema.Content
  ( Unique
    ( UniqueSubmissionPolicyVariant
    )
  , EntityField
    ( RecordAssignedSubmissionPolicyEditor
    , RecordAssignedSubmissionPolicyPolicy
    , RecordSubmissionApprovalEditor
    , RecordSubmissionApprovalRecord
    , StoredRecordSubmissionVariant
    )
  , RecordAssignedSubmissionPolicy (..)
  , RecordSubmissionPolicy (..)
  , StoredRecordSubmission (..), StoredRecordSubmissionId
  , RecordSubmissionApproval (..)
  )

import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Monoid ((<>))
import Data.Time (getCurrentTime)
import Data.Aeson.JSONTuple (JSONTuple (..))
import Data.Aeson.JSONUnit (JSONUnit (..))
import Control.Monad (forM, void)
import Control.Monad.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Logging (warn')
import Database.Persist (Entity (..), (==.), (=.))
import Database.Persist.Sql (SqlBackend, runSqlPool)
import Database.Persist.Class (selectList, getBy, insert_, update, get, delete)



data ValidateEditorError
  = EditorInvalidNoName


validateEditor :: SetEditor -> ReaderT SqlBackend IO (Either ValidateEditorError EditorValid)
validateEditor SetEditor{..} = do
  case setEditorName of
    Nothing -> pure (Left EditorInvalidNoName)
    Just name -> pure $ Right EditorValid
      { editorValidName = name
      }


getEditor :: AuthToken -> SystemM (UserExists (HasRole (Maybe GetEditor)))
getEditor authToken =
  verifyRole Editor authToken $ \userId -> liftDb $ do
    mCustEnt <- getBy (UniqueEditor userId)
    case mCustEnt of
      Nothing -> pure Nothing
      Just (Entity editorId (StoredEditor _ name)) -> do
        variants <- do
          xs <- selectList [RecordAssignedSubmissionPolicyEditor ==. editorId] []
          fmap catMaybes $ forM xs $ \(Entity _ (RecordAssignedSubmissionPolicy policyId _)) -> do
            mPolicy <- get policyId
            case mPolicy of
              Nothing -> pure Nothing
              Just (RecordSubmissionPolicy v _) -> pure (Just v)
        approved <- do
          xs <- selectList [RecordSubmissionApprovalEditor ==. editorId] []
          forM xs $ \(Entity approvalId _) -> pure approvalId
        pure $ Just $ GetEditor name variants approved


unsafeStoreEditor :: StoredUserId -> EditorValid -> ReaderT SqlBackend IO ()
unsafeStoreEditor userId EditorValid{..} = do
  mCustEnt <- getBy (UniqueEditor userId)
  case mCustEnt of
    Nothing -> do
      insert_ (StoredEditor userId editorValidName)
    Just (Entity editorId _) ->
      update editorId [StoredEditorName =. editorValidName]


setEditor :: AuthToken -> SetEditor -> SystemM (UserExists (HasRole JSONUnit))
setEditor authToken setEditor' =
  verifyRole Editor authToken $ \userId -> liftDb $ do
    now <- liftIO getCurrentTime
    let record = ProfileRecord (ProfileRecordEditor setEditor')
    insert_ $ StoredRecordSubmission userId now record (contentRecordVariant record)
    pure JSONUnit


getSubmissionPolicy :: AuthToken
                    -> ContentRecordVariant
                    -> SystemM
                       ( UserExists
                         ( HasRole
                           ( SubmissionPolicy GetRecordSubmissionPolicy)))
getSubmissionPolicy authToken variant =
  verifyRole Editor authToken $ \_ -> liftDb $ do
    mPolicy <- getBy (UniqueSubmissionPolicyVariant variant)
    case mPolicy of
      Nothing -> pure NoSubmissionPolicy
      Just (Entity policyId (RecordSubmissionPolicy _ additional)) -> do
        assigned <- do
          xs <- selectList [RecordAssignedSubmissionPolicyPolicy ==. policyId] []
          pure $ (\(Entity _ (RecordAssignedSubmissionPolicy _ e)) -> e) <$> xs
        pure $ SubmissionPolicy $ GetRecordSubmissionPolicy variant additional assigned


isApprovedSubmission :: AuthToken
                     -> StoredRecordSubmissionId
                     -> SystemM
                        ( UserExists
                          ( HasRole
                            ( SubmissionExists
                              ( SubmissionPolicy Bool))))
isApprovedSubmission authToken submissionId =
  verifyRole Editor authToken $ \_ -> liftDb $ do
    mSubmission <- get submissionId
    case mSubmission of
      Nothing -> pure SubmissionDoesntExist
      Just (StoredRecordSubmission _ _ _ variant) -> fmap SubmissionExists $ do
        mPolicy <- getBy (UniqueSubmissionPolicyVariant variant)
        case mPolicy of
          Nothing -> pure NoSubmissionPolicy
          Just (Entity policyId (RecordSubmissionPolicy _ additional)) ->
            fmap SubmissionPolicy $ do
              assignees <- do
                xs <- selectList [RecordAssignedSubmissionPolicyPolicy ==. policyId] []
                pure $ Set.fromList $ (\(Entity _ (RecordAssignedSubmissionPolicy _ e)) -> e) <$> xs
              approved <- do
                as <- selectList [RecordSubmissionApprovalRecord ==. submissionId] []
                pure $ Set.fromList $ (\(Entity _ (RecordSubmissionApproval _ editor)) -> editor) <$> as
              let needingApproval = assignees `Set.difference` approved
                  extraApproval = approved `Set.difference` assignees
              pure $ Set.null needingApproval && Set.size extraApproval >= additional


integrateRecord :: AuthToken
                -> StoredRecordSubmissionId
                -> SystemM
                   ( UserExists
                     ( HasRole
                       ( SubmissionExists
                         ( SubmissionPolicy Bool))))
integrateRecord authToken submissionId = do
  mIsVerified <- isApprovedSubmission authToken submissionId
  case mIsVerified of
    UserDoesntExist -> pure UserDoesntExist
    UserExists DoesntHaveRole -> pure $ UserExists DoesntHaveRole
    UserExists (HasRole SubmissionDoesntExist) ->
      pure $ UserExists $ HasRole SubmissionDoesntExist
    UserExists (HasRole (SubmissionExists NoSubmissionPolicy)) ->
      pure $ UserExists $ HasRole $ SubmissionExists NoSubmissionPolicy
    UserExists (HasRole (SubmissionExists (SubmissionPolicy isVerified))) ->
      if not isVerified
        then pure $ UserExists $ HasRole $ SubmissionExists $ SubmissionPolicy False
        else do
          mUserRec <- liftDb $ do
            mSub <- get submissionId
            case mSub of
              Nothing -> pure $ UserExists $ HasRole SubmissionDoesntExist
              Just (StoredRecordSubmission author _ record _) ->
                pure $ UserExists $ HasRole $ SubmissionExists (author, record)
          case mUserRec of
            UserDoesntExist -> pure UserDoesntExist
            UserExists DoesntHaveRole -> pure $ UserExists DoesntHaveRole
            UserExists (HasRole SubmissionDoesntExist) ->
              pure $ UserExists $ HasRole SubmissionDoesntExist
            UserExists (HasRole (SubmissionExists (userId, record))) -> do
              case record of
                TagRecord tagRecord -> case tagRecord of
                  TagRecordChef chefTag             -> unsafeStoreChefTag chefTag
                  TagRecordCulture cultureTag       -> unsafeStoreCultureTag cultureTag
                  TagRecordDiet dietTag             -> unsafeStoreDietTag dietTag
                  TagRecordFarm farmTag             -> unsafeStoreFarmTag farmTag
                  TagRecordIngredient ingredientTag -> unsafeStoreIngredientTag ingredientTag
                  TagRecordMeal mealTag             -> unsafeStoreMealTag mealTag
                ChefRecord chefRecord -> case chefRecord of
                  ChefRecordNewMenu newMenu -> void $ unsafeStoreNewMenu userId newMenu
                  ChefRecordSetMenu setMenu -> void $ unsafeStoreSetMenu userId setMenu
                  ChefRecordNewMeal newMeal -> void $ unsafeStoreNewMeal userId newMeal
                  ChefRecordSetMeal setMeal -> void $ unsafeStoreSetMeal userId setMeal
                ProfileRecord profileRecord -> case profileRecord of
                  ProfileRecordChef setChef -> do
                    mChefValid <- liftDb $ validateChef setChef
                    case mChefValid of
                      Left _ -> pure () -- FIXME error somehow?
                      Right chefValid -> void $ unsafeStoreChef userId chefValid
                  ProfileRecordCustomer setCustomer -> do
                    mCustomerValid <- liftDb $ validateCustomer setCustomer
                    case mCustomerValid of
                      Left _ -> pure () -- FIXME error somehow?
                      Right customerValid -> void $ unsafeStoreCustomer userId customerValid
                  ProfileRecordEditor setEditor -> do
                    mEditorValid <- liftDb $ validateEditor setEditor
                    case mEditorValid of
                      Left _ -> pure () -- FIXME error somehow?
                      Right editorValid -> void $ liftDb $ unsafeStoreEditor userId editorValid
              liftDb $ delete submissionId
              pure $ UserExists $ HasRole $ SubmissionExists $ SubmissionPolicy True


getSubmissions :: AuthToken
               -> ContentRecordVariant
               -> SystemM
                  ( UserExists
                    ( HasRole [JSONTuple StoredRecordSubmissionId GetRecordSubmission]))
getSubmissions authToken variant = do
  verifyRole Editor authToken $ \_ -> liftDb $ do
    xs <- selectList [StoredRecordSubmissionVariant ==. variant] []
    forM xs $ \(Entity submissionId (StoredRecordSubmission author timestamp content _)) -> do
      approvals <- do
        ys <- selectList [RecordSubmissionApprovalRecord ==. submissionId] []
        pure $ (\(Entity _ (RecordSubmissionApproval _ e)) -> e) <$> ys
      pure $ JSONTuple submissionId $ GetRecordSubmission author timestamp content approvals



approveSubmission :: AuthToken
                  -> StoredRecordSubmissionId
                  -> SystemM
                     ( UserExists
                       ( HasRole
                         ( EditorExists
                           ( SubmissionExists
                             ( SubmissionPolicy JSONUnit)))))
approveSubmission authToken submissionId = do
  mVariant <- verifyRole Editor authToken $ \userId -> do
    mEditor <- liftDb $ getBy (UniqueEditor userId)
    case mEditor of
      Nothing -> pure EditorDoesntExist
      Just (Entity editorId _) -> fmap EditorExists $ do
        mSubmission <- liftDb $ get submissionId
        case mSubmission of
          Nothing -> pure SubmissionDoesntExist
          Just (StoredRecordSubmission _ _ _ variant) ->
            pure $ SubmissionExists (variant, editorId)
  case mVariant of
    UserDoesntExist -> pure UserDoesntExist
    UserExists DoesntHaveRole -> pure (UserExists DoesntHaveRole)
    UserExists (HasRole EditorDoesntExist) ->
      pure $ UserExists $ HasRole EditorDoesntExist
    UserExists (HasRole (EditorExists SubmissionDoesntExist)) ->
      pure $ UserExists $ HasRole $ EditorExists SubmissionDoesntExist
    UserExists (HasRole (EditorExists (SubmissionExists (variant, editorId)))) -> do
      mPolicy <- getSubmissionPolicy authToken variant
      case mPolicy of
        UserDoesntExist -> pure UserDoesntExist
        UserExists DoesntHaveRole -> pure (UserExists DoesntHaveRole)
        UserExists (HasRole NoSubmissionPolicy) ->
          pure $ UserExists $ HasRole $ EditorExists $ SubmissionExists $ NoSubmissionPolicy
        UserExists (HasRole (SubmissionPolicy _)) ->
          fmap (UserExists . HasRole . EditorExists . SubmissionExists . SubmissionPolicy) $ do
            liftDb $ insert_ $ RecordSubmissionApproval submissionId editorId
            void (integrateRecord authToken submissionId)
            pure JSONUnit

