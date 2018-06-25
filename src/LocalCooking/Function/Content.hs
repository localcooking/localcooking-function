{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  , DeriveGeneric
  , OverloadedStrings
  #-}

module LocalCooking.Function.Content where

import LocalCooking.Function.Tag (unsafeStoreChefTag, unsafeStoreCultureTag, unsafeStoreDietTag, unsafeStoreFarmTag, unsafeStoreIngredientTag, unsafeStoreMealTag)
import LocalCooking.Function.Chef (unsafeSetChef, unsafeNewMenu, unsafeSetMenu, unsafeNewMeal, unsafeSetMeal)
import LocalCooking.Function.System (SystemM, SystemEnv (..), getUserId, guardRole, getSystemEnv)
import LocalCooking.Semantics.Common (WithId (..))
import LocalCooking.Semantics.Content (SetEditor (..), GetEditor (..))
import LocalCooking.Semantics.ContentRecord
  ( ContentRecord (..), TagRecord (..), ChefRecord (..)
  , contentRecordVariant, ContentRecordVariant)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.User.Role (UserRole (Editor))
import LocalCooking.Database.Schema
  ( StoredEditor (..), StoredEditorId
  , EntityField
    ( StoredEditorStoredEditorName
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
    ( RecordAssignedSubmissionPolicyRecordAssignedSubmissionPolicyEditor
    , RecordAssignedSubmissionPolicyRecordAssignedSubmissionPolicy
    , RecordSubmissionApprovalRecordSubmissionApprovalEditor
    , RecordSubmissionApprovalRecordSubmissionApprovalRecord
    )
  , RecordAssignedSubmissionPolicy (..)
  , RecordSubmissionPolicy (..), RecordSubmissionPolicyId
  , StoredRecordSubmission (..), StoredRecordSubmissionId
  , RecordSubmissionApproval (..)
  )

import Data.Maybe (catMaybes)
import Data.Time (getCurrentTime)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Monoid ((<>))
import Control.Monad (forM, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Logging (warn')
import Database.Persist (Entity (..), (==.), (=.))
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Class (selectList, getBy, insert_, update, get, delete)




setEditor :: AuthToken -> SetEditor -> SystemM Bool
setEditor authToken SetEditor{..} = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure False
    Just userId -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        mCustEnt <- getBy (UniqueEditor userId)
        case mCustEnt of
          Nothing -> do
            insert_ (StoredEditor userId setEditorName)
          Just (Entity editorId _) ->
            update editorId [StoredEditorStoredEditorName =. setEditorName]
        pure True


getEditor :: AuthToken -> SystemM (Maybe GetEditor)
getEditor authToken = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure Nothing
    Just userId -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        mCustEnt <- getBy (UniqueEditor userId)
        case mCustEnt of
          Nothing -> pure Nothing
          Just (Entity editorId (StoredEditor _ name)) -> do
            variants <- do
              xs <- selectList [RecordAssignedSubmissionPolicyRecordAssignedSubmissionPolicyEditor ==. editorId] []
              fmap catMaybes $ forM xs $ \(Entity _ (RecordAssignedSubmissionPolicy policyId _)) -> do
                mPolicy <- get policyId
                case mPolicy of
                  Nothing -> pure Nothing
                  Just (RecordSubmissionPolicy v _) -> pure (Just v)
            approved <- do
              xs <- selectList [RecordSubmissionApprovalRecordSubmissionApprovalEditor ==. editorId] []
              forM xs $ \(Entity approvalId _) -> pure approvalId
            pure $ Just $ GetEditor name variants approved


-- NOTE Written in each authoring module - this module uses their unsafe defs
-- submitRecord :: AuthToken -> ContentRecord -> SystemM Bool
-- submitRecord authToken record = do
--   mUserId <- getUserId authToken
--   case mUserId of
--     Nothing -> pure False
--     Just userId -> do
--       SystemEnv{systemEnvDatabase} <- getSystemEnv
--       liftIO $ flip runSqlPool systemEnvDatabase $ do
--         now <- liftIO getCurrentTime
--         insert_ (StoredRecordSubmission userId now record)
--         pure True


getSubmissionPolicy :: AuthToken -> ContentRecordVariant -> SystemM (Maybe (WithId RecordSubmissionPolicyId RecordSubmissionPolicy))
getSubmissionPolicy authToken variant = do
  mEditor <- verifyEditorhood authToken
  case mEditor of
    Nothing -> pure Nothing
    Just _ -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      flip runSqlPool systemEnvDatabase $ do
        mPolicy <- getBy (UniqueSubmissionPolicyVariant variant)
        case mPolicy of
          Nothing -> do
            warn' $ "No stored submission policy for variant: " <> T.pack (show variant)
            pure Nothing
          Just (Entity policyId policy) -> pure $ Just $ WithId policyId policy


isApprovedSubmission :: AuthToken -> StoredRecordSubmissionId -> SystemM (Maybe Bool)
isApprovedSubmission authToken submissionId = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv
  mVariant <- flip runSqlPool systemEnvDatabase $ do
    mSubmission <- get submissionId
    case mSubmission of
      Nothing -> pure Nothing
      Just (StoredRecordSubmission _ _ content) -> do
        pure $ Just $ contentRecordVariant content
  case mVariant of
    Nothing -> pure Nothing
    Just variant -> do
      mPolicy <- getSubmissionPolicy authToken variant
      case mPolicy of
        Nothing -> pure Nothing
        Just (WithId policyId (RecordSubmissionPolicy _ additional)) -> do
          flip runSqlPool systemEnvDatabase $ do
            assignees <- do
              as <- selectList [RecordAssignedSubmissionPolicyRecordAssignedSubmissionPolicy ==. policyId] []
              pure $ Set.fromList $ (\(Entity _ (RecordAssignedSubmissionPolicy _ editor)) -> editor) <$> as
            approved <- do
              as <- selectList [RecordSubmissionApprovalRecordSubmissionApprovalRecord ==. submissionId] []
              pure $ Set.fromList $ (\(Entity _ (RecordSubmissionApproval _ editor)) -> editor) <$> as
            let needingApproval = assignees `Set.difference` approved
                extraApproval = approved `Set.difference` assignees
            pure $ Just $
              if Set.null needingApproval
                then Set.size extraApproval >= additional
                else False


integrateRecord :: AuthToken -> StoredRecordSubmissionId -> SystemM Bool
integrateRecord authToken submissionId = do
  mIsVerified <- isApprovedSubmission authToken submissionId
  case mIsVerified of
    Nothing -> pure False
    Just isVerified
      | not isVerified -> pure False
      | otherwise -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      mUserRec <- liftIO $ flip runSqlPool systemEnvDatabase $ do
        mSub <- get submissionId
        case mSub of
          Nothing -> pure Nothing
          Just (StoredRecordSubmission author _ record) -> pure $ Just (author, record)
      case mUserRec of
        Nothing -> pure False
        Just (userId, record) -> do
          case record of
            TagRecord tagRecord -> case tagRecord of
              TagRecordChef chefTag             -> unsafeStoreChefTag chefTag
              TagRecordCulture cultureTag       -> unsafeStoreCultureTag cultureTag
              TagRecordDiet dietTag             -> unsafeStoreDietTag dietTag
              TagRecordFarm farmTag             -> unsafeStoreFarmTag farmTag
              TagRecordIngredient ingredientTag -> unsafeStoreIngredientTag ingredientTag
              TagRecordMeal mealTag             -> unsafeStoreMealTag mealTag
            ChefRecord chefRecord -> case chefRecord of
              ChefRecordChef getSetChef -> void $ unsafeSetChef userId getSetChef
              ChefRecordNewMenu newMenu -> void $ unsafeNewMenu userId newMenu
              ChefRecordSetMenu setMenu -> void $ unsafeSetMenu userId setMenu
              ChefRecordNewMeal newMeal -> void $ unsafeNewMeal userId newMeal
              ChefRecordSetMeal setMeal -> void $ unsafeSetMeal userId setMeal
          flip runSqlPool systemEnvDatabase $ delete submissionId
          pure True



approveSubmission :: AuthToken -> StoredRecordSubmissionId -> SystemM Bool
approveSubmission authToken submissionId = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv
  mEditor <- verifyEditorhood authToken
  case mEditor of
    Nothing -> pure False
    Just editorId -> do
      mVariant <- flip runSqlPool systemEnvDatabase $ do
        mSubmission <- get submissionId
        case mSubmission of
          Nothing -> pure Nothing
          Just (StoredRecordSubmission _ _ content) -> do
            pure $ Just $ contentRecordVariant content
      case mVariant of
        Nothing -> pure False
        Just variant -> do
          mPolicy <- getSubmissionPolicy authToken variant
          case mPolicy of
            Nothing -> pure False
            Just _ -> do
              flip runSqlPool systemEnvDatabase $
                insert_ $ RecordSubmissionApproval submissionId editorId
              void (integrateRecord authToken submissionId)
              pure True



verifyEditorhood :: AuthToken -> SystemM (Maybe StoredEditorId)
verifyEditorhood authToken = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure Nothing
    Just userId -> do
      isEditor <- guardRole userId Editor
      if not isEditor
        then pure Nothing
        else do
          SystemEnv{systemEnvDatabase} <- getSystemEnv
          flip runSqlPool systemEnvDatabase $ do
            mEnt <- getBy (UniqueEditor userId)
            case mEnt of
              Nothing -> pure Nothing
              Just (Entity editorId _) -> pure (Just editorId)
