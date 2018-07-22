{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  , DeriveGeneric
  , OverloadedStrings
  #-}

module LocalCooking.Function.Blog
  ( getBlogPostCategories
  , getBlogPostCategory
  , newBlogPostCategory
  , setBlogPostCategory
  , getBlogPosts
  , getBlogPost
  , newBlogPost
  , setBlogPost
  ) where

import LocalCooking.Function.System
  (SystemM, SystemEnv (..), getUserId, getSystemEnv)
import LocalCooking.Semantics.Blog
  ( GetBlogPost (GetBlogPost), NewBlogPost (NewBlogPost), SetBlogPost (SetBlogPost)
  , BlogPostSynopsis (..), BlogPostCategorySynopsis (..), GetBlogPostCategory (..)
  , SetBlogPostCategory (..), NewBlogPostCategory (..)
  )
import LocalCooking.Semantics.Common (WithId (..))
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.User.Role (UserRole (Editor))
import LocalCooking.Database.Schema
  ( hasRole, StoredBlogPost (..), StoredBlogPostCategory (..), StoredBlogPostPrimary (..)
  , StoredBlogPostId, StoredEditor (..)
  , StoredBlogPostCategoryId
  , Unique
    ( UniqueBlogPost, UniqueBlogPostCategory, UniqueEditor
    )
  , EntityField
    ( StoredBlogPostHeadline, StoredBlogPostPermalink, StoredBlogPostContent
    , StoredBlogPostCategory', StoredBlogPostPriority, StoredBlogPostCategoryPriority
    , StoredBlogPostCategoryPriority, StoredBlogPostPrimaryCategory
    )
  )

import Data.Maybe (catMaybes)
import Data.Time (getCurrentTime)
import Data.Text.Permalink (Permalink)
import Control.Monad (forM_, forM)
import Control.Monad.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.Persist (Entity (..), (==.), (=.), SelectOpt (Asc))
import Database.Persist.Sql (SqlBackend, runSqlPool)
import Database.Persist.Class
  (selectList, selectFirst, getBy, insert, insert_, update, get)



-- * Category


getBlogPostCategories :: SystemM [BlogPostCategorySynopsis]
getBlogPostCategories = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv
  flip runSqlPool systemEnvDatabase $ do
    xs <- selectList [] [Asc StoredBlogPostCategoryPriority]
    pure $ (\(Entity _ (StoredBlogPostCategory name priority permalink)) ->
             BlogPostCategorySynopsis name permalink priority) <$> xs

getBlogPostCategory :: Permalink -> SystemM (Maybe GetBlogPostCategory)
getBlogPostCategory permalink = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv
  liftIO $ flip runSqlPool systemEnvDatabase $ do
    mEnt <- getBy (UniqueBlogPostCategory permalink)
    case mEnt of
      Nothing -> pure Nothing
      Just (Entity categoryId (StoredBlogPostCategory name _ permalink)) -> do
        primary <- do
          mPrimary <- selectFirst [StoredBlogPostPrimaryCategory ==. categoryId] []
          case mPrimary of
            Nothing -> pure Nothing
            Just (Entity _ (StoredBlogPostPrimary primaryPost _)) -> getBlogPostSynopsisById primaryPost
        posts <- do
          xs <- selectList [StoredBlogPostCategory' ==. categoryId] [Asc StoredBlogPostPriority]
          fmap catMaybes $ forM xs $ \(Entity postId _) -> getBlogPostSynopsisById postId
        pure $ Just $ GetBlogPostCategory name permalink primary posts categoryId

newBlogPostCategory :: NewBlogPostCategory -> SystemM (Maybe StoredBlogPostCategoryId)
newBlogPostCategory = undefined

setBlogPostCategory :: SetBlogPostCategory -> SystemM Bool
setBlogPostCategory = undefined


-- * Post

getBlogPosts :: StoredBlogPostCategoryId -> SystemM [BlogPostSynopsis]
getBlogPosts = undefined

getBlogPost :: StoredBlogPostCategoryId -> Permalink -> SystemM (Maybe GetBlogPost)
getBlogPost = undefined

newBlogPost :: AuthToken -> NewBlogPost -> SystemM (Maybe StoredBlogPostId)
newBlogPost = undefined

setBlogPost :: AuthToken -> SetBlogPost -> SystemM Bool
setBlogPost = undefined



-- * Unexposed

getBlogPostSynopsisById :: StoredBlogPostId -> ReaderT SqlBackend IO (Maybe BlogPostSynopsis)
getBlogPostSynopsisById k = do
  mPost <- get k
  case mPost of
    Nothing -> pure Nothing
    Just (StoredBlogPost author timestamp headline permalink _ variant priority _) -> do
      mEditor <- get author
      case mEditor of
        Nothing -> pure Nothing
        Just (StoredEditor _ name) ->
          pure $ Just $
            BlogPostSynopsis name timestamp headline permalink variant priority
