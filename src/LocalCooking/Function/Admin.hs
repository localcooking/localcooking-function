{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  #-}

module LocalCooking.Function.Admin
  ( getUsers, setUser, addUser
  , getSubmissionPolicy, setSubmissionPolicy, assignSubmissionPolicy
  ) where

import LocalCooking.Semantics.Common (User (..), SocialLoginForm (..))
import LocalCooking.Semantics.Admin (SetUser (..), NewUser (..), GetSetSubmissionPolicy (..))
import LocalCooking.Function.System (SystemM, SystemEnv (..), getUserId, getSystemEnv)
import LocalCooking.Database.Schema
  ( hasRole, FacebookUserDetails (..)
  , StoredUser (..)
  , UserRoleStored (..)
  , StoredEditorId
  , EntityField
    ( StoredUserEmail, StoredUserCreated, StoredUserConfirmed, StoredUserPassword
    , UserRoleStoredOwner
    )
  , Unique (UniqueEmail, UniqueFacebookUserDetailsOwner)
  )
import LocalCooking.Database.Schema.Content
  ( EntityField
    ( RecordSubmissionPolicyAdditional
    , RecordAssignedSubmissionPolicyPolicy
    )
  , Unique
    ( UniqueSubmissionPolicyVariant
    )
  , RecordAssignedSubmissionPolicy (..)
  , RecordSubmissionPolicy (..)
  )
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.User.Role (UserRole (Admin))
import LocalCooking.Semantics.ContentRecord.Variant (ContentRecordVariant)

import Data.IORef (newIORef, readIORef, modifyIORef)
import qualified Data.Set as Set
import Data.Time (getCurrentTime)
import Control.Monad (forM, forM_, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.Persist (Entity (..), (==.), (=.))
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Class (selectList, getBy, insert, insert_, delete, deleteBy, update)



-- | Witness all User data-views, from an authenticated Admin
getUsers :: AuthToken -> SystemM (Maybe [User])
getUsers authToken = do
  isAuthorized <- verifyAdminhood authToken
  if not isAuthorized
    then pure Nothing
    else do
      SystemEnv{systemEnvDatabase} <- getSystemEnv

      liftIO $ flip runSqlPool systemEnvDatabase $ do
        xs <- selectList [] []
        fmap Just $ forM xs $ \(Entity k (StoredUser created email _ conf)) -> do
          roles <- fmap (fmap (\(Entity _ (UserRoleStored r _)) -> r))
                $ selectList [UserRoleStoredOwner ==. k] []
          mFb <- getBy (UniqueFacebookUserDetailsOwner k)
          pure User
            { userId = k
            , userCreated = created
            , userEmail = email
            , userSocialLogin = SocialLoginForm
              { socialLoginFormFb = case mFb of
                Nothing -> Nothing
                Just (Entity _ (FacebookUserDetails uId _)) -> Just uId
              }
            , userEmailConfirmed = conf
            , userRoles = roles
            }


-- | Assigned new user details with a SetUser data-view
setUser :: AuthToken -> SetUser -> SystemM Bool
setUser authToken x = do
  isAuthorized <- verifyAdminhood authToken
  if not isAuthorized
    then pure False
    else case x of
      SetUserDelete User{userId} -> do
        SystemEnv{systemEnvDatabase} <- getSystemEnv
        liftIO $ flip runSqlPool systemEnvDatabase $ do
          delete userId
          pure True
      SetUserUpdate{setUserUpdateUser = User{..}, setUserUpdateNewPassword} -> do
        SystemEnv{systemEnvDatabase} <- getSystemEnv

        liftIO $ flip runSqlPool systemEnvDatabase $ do
          update userId $
            [ StoredUserCreated =. userCreated
            , StoredUserEmail =. userEmail
            , StoredUserConfirmed =. userEmailConfirmed
            ] ++ case setUserUpdateNewPassword of
                  Nothing -> []
                  Just newPassword ->
                    [StoredUserPassword =. newPassword]
          case userSocialLogin of
            SocialLoginForm mFb -> do
              case mFb of
                Nothing -> deleteBy (UniqueFacebookUserDetailsOwner userId)
                Just userFb -> insert_ (FacebookUserDetails userFb userId)
          xs <- selectList [UserRoleStoredOwner ==. userId] []
          rolesRef <- liftIO (newIORef (Set.fromList userRoles))
          forM_ xs $ \(Entity k (UserRoleStored role _)) -> do
            roles' <- liftIO (readIORef rolesRef)
            if Set.member role roles'
              then liftIO $ modifyIORef rolesRef $ Set.delete role
              else delete k
          rolesLeft <- liftIO (readIORef rolesRef)
          forM_ rolesLeft $ \role -> insert_ (UserRoleStored role userId)

          pure True


-- | Register a new user with the NewUser data-view
addUser :: AuthToken -> NewUser -> SystemM Bool
addUser authToken NewUser{..} = do
  isAuthorized <- verifyAdminhood authToken
  if not isAuthorized
    then pure False
    else do
      SystemEnv{systemEnvDatabase} <- getSystemEnv

      liftIO $ flip runSqlPool systemEnvDatabase $ do
        mEnt <- getBy (UniqueEmail newUserEmail)
        case mEnt of
          Just _ -> pure False
          Nothing -> do
            now <- liftIO getCurrentTime
            insert_ (StoredUser now newUserEmail newUserPassword False)
            pure True


-- | Witness the GetSetSubmissionPolicy data-view for a specific ContentRecordVariant
getSubmissionPolicy :: AuthToken -> ContentRecordVariant -> SystemM (Maybe GetSetSubmissionPolicy)
getSubmissionPolicy authToken variant = do
  isAdmin <- verifyAdminhood authToken
  if not isAdmin
    then pure Nothing
    else do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        mEnt <- getBy (UniqueSubmissionPolicyVariant variant)
        case mEnt of
          Nothing -> pure Nothing
          Just (Entity policyId (RecordSubmissionPolicy _ additional)) -> do
            assigned <- do
              xs <- selectList [RecordAssignedSubmissionPolicyPolicy ==. policyId] []
              pure $ (\(Entity _ (RecordAssignedSubmissionPolicy _ entityId)) -> entityId) <$> xs
            pure $ Just $ GetSetSubmissionPolicy variant additional assigned


-- | Physically adjust the GetSetSubmissionPolicy data-view on the database
setSubmissionPolicy :: AuthToken -> GetSetSubmissionPolicy -> SystemM Bool
setSubmissionPolicy authToken (GetSetSubmissionPolicy variant additional assigned) = do
  isAdmin <- verifyAdminhood authToken
  if not isAdmin
    then pure False
    else do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        mEnt <- getBy (UniqueSubmissionPolicyVariant variant)
        case mEnt of
          Just (Entity p _) -> do
            update p [RecordSubmissionPolicyAdditional =. additional]
          Nothing -> do
            insert_ (RecordSubmissionPolicy variant additional)
      forM_ assigned $ \editorId ->
        void $ assignSubmissionPolicy authToken editorId variant
      pure True


-- | Assign an editor to be a manual content reviewer for a ContentRecordVariant
assignSubmissionPolicy :: AuthToken -> StoredEditorId -> ContentRecordVariant -> SystemM Bool
assignSubmissionPolicy authToken editorId variant = do
  isAdmin <- verifyAdminhood authToken
  if not isAdmin
    then pure False
    else do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ do
        mEnt <- getBy (UniqueSubmissionPolicyVariant variant)
        case mEnt of
          Nothing -> pure False
          Just (Entity policyId _) -> do
            insert_ (RecordAssignedSubmissionPolicy policyId editorId)
            pure True



-- * Utilities


-- | Verify that a login session belongs to an authentic Admin according to
--   the user's Role
verifyAdminhood :: AuthToken -> SystemM Bool
verifyAdminhood authToken = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure False
    Just userId -> do
      SystemEnv{systemEnvDatabase} <- getSystemEnv
      liftIO $ flip runSqlPool systemEnvDatabase $ hasRole userId Admin
