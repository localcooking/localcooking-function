{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  , DeriveGeneric
  , OverloadedStrings
  #-}

module LocalCooking.Function.Content where

import LocalCooking.Function.Tag (unsafeStoreChefTag, unsafeStoreCultureTag, unsafeStoreDietTag, unsafeStoreFarmTag, unsafeStoreIngredientTag, unsafeStoreMealTag)
import LocalCooking.Function.Chef
  ( unsafeStoreChef, unsafeStoreNewMenu, unsafeStoreSetMenu
  , unsafeStoreNewMeal, unsafeStoreSetMeal, validateChef
  )
import LocalCooking.Function.System (SystemM, SystemEnv (..), getUserId, getSystemEnv)
import LocalCooking.Semantics.Common (WithId (..))
import LocalCooking.Semantics.Content
  ( SetEditor (..), GetRecordSubmissionPolicy (..))
import LocalCooking.Semantics.Content.Approval
  ( GetEditor (..), GetRecordSubmission (..))
import LocalCooking.Semantics.ContentRecord
  ( ContentRecord (..), TagRecord (..), ChefRecord (..), ProfileRecord (..))
import LocalCooking.Semantics.ContentRecord.Variant (ContentRecordVariant)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.User.Role (UserRole (Editor))
import LocalCooking.Database.Schema
  ( hasRole, StoredEditor (..), StoredEditorId
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
            update editorId [StoredEditorName =. setEditorName]
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



getSubmissionPolicy :: AuthToken -> ContentRecordVariant -> SystemM (Maybe GetRecordSubmissionPolicy)
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
          Just (Entity policyId (RecordSubmissionPolicy _ additional)) -> do
            assigned <- do
              xs <- selectList [RecordAssignedSubmissionPolicyPolicy ==. policyId] []
              pure $ (\(Entity _ (RecordAssignedSubmissionPolicy _ e)) -> e) <$> xs
            pure $ Just $ GetRecordSubmissionPolicy variant additional assigned


isApprovedSubmission :: AuthToken -> StoredRecordSubmissionId -> SystemM (Maybe Bool)
isApprovedSubmission authToken submissionId = do
  mEditor <- verifyEditorhood authToken
  case mEditor of
    Nothing -> pure Nothing
    Just _ -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        mSubmission <- get submissionId
        case mSubmission of
          Nothing -> pure Nothing
          Just (StoredRecordSubmission _ _ _ variant) -> do
            mPolicy <- getBy (UniqueSubmissionPolicyVariant variant)
            case mPolicy of
              Nothing -> pure Nothing
              Just (Entity policyId (RecordSubmissionPolicy _ additional)) -> do
                assignees <- do
                  xs <- selectList [RecordAssignedSubmissionPolicyPolicy ==. policyId] []
                  pure $ Set.fromList $ (\(Entity _ (RecordAssignedSubmissionPolicy _ e)) -> e) <$> xs
                approved <- do
                  as <- selectList [RecordSubmissionApprovalRecord ==. submissionId] []
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
          Just (StoredRecordSubmission author _ record _) -> pure $ Just (author, record)
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
              ChefRecordNewMenu newMenu -> void $ unsafeStoreNewMenu userId newMenu
              ChefRecordSetMenu setMenu -> void $ unsafeStoreSetMenu userId setMenu
              ChefRecordNewMeal newMeal -> void $ unsafeStoreNewMeal userId newMeal
              ChefRecordSetMeal setMeal -> void $ unsafeStoreSetMeal userId setMeal
            ProfileRecord profileRecord -> case profileRecord of
              ProfileRecordChef setChef -> do
                mChefValid <- liftIO $ flip runSqlPool systemEnvDatabase $
                  validateChef setChef
                case mChefValid of
                  Left _ -> pure () -- FIXME error somehow?
                  Right chefValid -> void $ unsafeStoreChef userId chefValid
            -- FIXME make total
          flip runSqlPool systemEnvDatabase $ delete submissionId
          pure True



getSubmissions :: AuthToken -> ContentRecordVariant -> SystemM (Maybe [WithId StoredRecordSubmissionId GetRecordSubmission])
getSubmissions authToken variant = do
  mEditor <- verifyEditorhood authToken
  case mEditor of
    Nothing -> pure Nothing
    Just _ -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        xs <- selectList [StoredRecordSubmissionVariant ==. variant] []
        fmap Just $ forM xs $ \(Entity submissionId (StoredRecordSubmission author timestamp content _)) -> do
          approvals <- do
            ys <- selectList [RecordSubmissionApprovalRecord ==. submissionId] []
            pure $ (\(Entity _ (RecordSubmissionApproval _ e)) -> e) <$> ys
          pure $ WithId submissionId $ GetRecordSubmission author timestamp content approvals



approveSubmission :: AuthToken -> StoredRecordSubmissionId -> SystemM Bool
approveSubmission authToken submissionId = do
  mEditor <- verifyEditorhood authToken
  case mEditor of
    Nothing -> pure False
    Just editorId -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      mVariant <- liftIO $ flip runSqlPool systemEnvDatabase $ do
        mSubmission <- get submissionId
        case mSubmission of
          Nothing -> pure Nothing
          Just (StoredRecordSubmission _ _ _ variant) -> do
            pure $ Just variant
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
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        isEditor <- hasRole userId Editor
        if not isEditor
          then pure Nothing
          else do
            mEnt <- getBy (UniqueEditor userId)
            case mEnt of
              Nothing -> pure Nothing
              Just (Entity editorId _) -> pure (Just editorId)
