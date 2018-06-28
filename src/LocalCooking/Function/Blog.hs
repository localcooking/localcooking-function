{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  , DeriveGeneric
  , OverloadedStrings
  #-}

module LocalCooking.Function.Blog where

import LocalCooking.Function.System
  (SystemM, SystemEnv (..), getUserId, guardRole, getSystemEnv)
import LocalCooking.Semantics.Blog
  ( GetBlogPost (..), NewBlogPost (..), SetBlogPost (..), BlogPostSynopsis (..)
  )
import LocalCooking.Semantics.Common (WithId (..))
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.User.Role (UserRole (Editor))
import LocalCooking.Database.Schema
  ( StoredBlogPost (..), StoredBlogPostId, StoredEditor (..)
  , Unique (UniqueBlogPost, UniqueEditor)
  , EntityField
    ( StoredBlogPostHeadline, StoredBlogPostPermalink, StoredBlogPostContent)
  )

import Data.Maybe (catMaybes)
import Data.Time (getCurrentTime)
import Data.Text.Permalink (Permalink)
import Control.Monad (forM_, forM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.Persist (Entity (..), (==.), (=.))
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Class (selectList, getBy, insert, insert_, update, get)



getBlogPost :: Permalink -> SystemM (Maybe GetBlogPost)
getBlogPost postId = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv
  liftIO $ flip runSqlPool systemEnvDatabase $ do
    mPost <- getBy (UniqueBlogPost postId)
    case mPost of
      Nothing -> pure Nothing
      Just (Entity _ (StoredBlogPost author timestamp headline permalink content)) -> do
        mEditor <- getBy (UniqueEditor author)
        case mEditor of
          Nothing -> pure Nothing
          Just (Entity _ (StoredEditor _ name)) ->
            pure $ Just $ GetBlogPost name timestamp headline permalink content


newBlogPost :: AuthToken -> NewBlogPost -> SystemM (Maybe StoredBlogPostId)
newBlogPost authToken NewBlogPost{..} = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure Nothing
    Just userId -> do
      isEditor <- guardRole userId Editor
      if not isEditor
        then pure Nothing
        else do
          now <- liftIO getCurrentTime
          SystemEnv{systemEnvDatabase} <- getSystemEnv
          flip runSqlPool systemEnvDatabase $ do
            mExists <- getBy (UniqueBlogPost newBlogPostPermalink)
            case mExists of
              Just _ -> pure Nothing
              Nothing -> do
                blogPostId <- insert $ StoredBlogPost
                  userId now newBlogPostHeadline newBlogPostPermalink newBlogPostContent
                pure (Just blogPostId)


setBlogPost :: AuthToken -> WithId StoredBlogPostId SetBlogPost -> SystemM Bool
setBlogPost authToken (WithId postId SetBlogPost{..}) = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure False
    Just userId -> do
      isEditor <- guardRole userId Editor
      if not isEditor
        then pure False
        else do
          SystemEnv{systemEnvDatabase} <- getSystemEnv
          flip runSqlPool systemEnvDatabase $ do
            mExists <- get postId
            case mExists of
              Nothing -> pure False
              Just (StoredBlogPost author _ _ _ _)
                | author /= userId -> pure False
                | otherwise -> do
                    update postId
                      [ StoredBlogPostHeadline =. setBlogPostHeadline
                      , StoredBlogPostPermalink =. setBlogPostPermalink
                      , StoredBlogPostContent =. setBlogPostContent
                      ]
                    pure True


getBlogPosts :: SystemM [WithId StoredBlogPostId BlogPostSynopsis]
getBlogPosts = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv
  flip runSqlPool systemEnvDatabase $ do
    xs <- selectList [] []
    fmap catMaybes $ forM xs $ \(Entity postId (StoredBlogPost a t h p _)) -> do
      mEditor <- getBy (UniqueEditor a)
      case mEditor of
        Nothing -> pure Nothing
        Just (Entity _ (StoredEditor _ name)) ->
          pure $ Just $ WithId postId $ BlogPostSynopsis name t h p
