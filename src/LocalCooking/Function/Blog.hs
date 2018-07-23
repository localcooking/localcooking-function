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
  (SystemM, SystemEnv (..), getUserId, getSystemEnv, liftDb)
import LocalCooking.Semantics.Blog
  ( GetBlogPost (GetBlogPost), NewBlogPost (NewBlogPost), SetBlogPost (SetBlogPost)
  , BlogPostSynopsis (..), BlogPostCategorySynopsis (..), GetBlogPostCategory (..)
  , SetBlogPostCategory (..), NewBlogPostCategory (..)
  )
import qualified LocalCooking.Semantics.Blog as Blog
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.User.Role (UserRole (Editor))
import LocalCooking.Database.Schema
  ( hasRole, StoredBlogPost (..), StoredBlogPostCategory (..), StoredBlogPostPrimary (..)
  , StoredBlogPostId, StoredEditor (..), StoredEditorId
  , StoredBlogPostCategoryId
  , Unique
    ( UniqueBlogPost, UniqueBlogPostCategory, UniqueEditor
    , UniquePrimaryBlogPost
    )
  , EntityField
    ( StoredBlogPostHeadline, StoredBlogPostPermalink, StoredBlogPostContent
    , StoredBlogPostCategory', StoredBlogPostPriority, StoredBlogPostCategoryPriority
    , StoredBlogPostCategoryPriority, StoredBlogPostPrimaryCategory
    , StoredBlogPostCategoryName, StoredBlogPostCategoryPermalink
    , StoredBlogPostPrimaryPost, StoredBlogPostVariant, StoredBlogPostAuthor
    )
  )

import Data.Maybe (catMaybes)
import Data.Time (getCurrentTime)
import Data.Text.Permalink (Permalink)
import Control.Monad (forM)
import Control.Monad.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.Persist (Entity (..), (==.), (=.), SelectOpt (Asc))
import Database.Persist.Sql (SqlBackend, runSqlPool)
import Database.Persist.Class
  (selectList, selectFirst, getBy, insert, insert_, update, get, deleteWhere)



-- * Category


getBlogPostCategories :: ReaderT SqlBackend IO [BlogPostCategorySynopsis]
getBlogPostCategories = do
  xs <- selectList [] [Asc StoredBlogPostCategoryPriority]
  pure $ (\(Entity _ (StoredBlogPostCategory name priority permalink)) ->
            BlogPostCategorySynopsis name permalink priority) <$> xs

getBlogPostCategory :: Permalink -> ReaderT SqlBackend IO (Maybe GetBlogPostCategory)
getBlogPostCategory permalink = do
  mEnt <- getBy (UniqueBlogPostCategory permalink)
  case mEnt of
    Nothing -> pure Nothing
    Just (Entity categoryId (StoredBlogPostCategory name _ _)) -> do
      primary <- do
        mPrimary <- selectFirst [StoredBlogPostPrimaryCategory ==. categoryId] []
        case mPrimary of
          Nothing -> pure Nothing
          Just (Entity _ (StoredBlogPostPrimary primaryPost _)) -> getBlogPostSynopsisById primaryPost
      posts <- do
        xs <- selectList [StoredBlogPostCategory' ==. categoryId] [Asc StoredBlogPostPriority]
        fmap catMaybes $ forM xs $ \(Entity postId _) -> getBlogPostSynopsisById postId
      pure $ Just $ GetBlogPostCategory name permalink primary posts categoryId

newBlogPostCategory :: NewBlogPostCategory -> ReaderT SqlBackend IO (Maybe StoredBlogPostCategoryId)
newBlogPostCategory NewBlogPostCategory{..} = do
  mEnt <- getBy (UniqueBlogPostCategory newBlogPostCategoryPermalink)
  case mEnt of
    Just _ -> pure Nothing
    Nothing -> do
      Just <$> insert (StoredBlogPostCategory newBlogPostCategoryName newBlogPostCategoryPriority newBlogPostCategoryPermalink)

setBlogPostCategory :: SetBlogPostCategory -> ReaderT SqlBackend IO Bool
setBlogPostCategory SetBlogPostCategory{..} = do
  mEnt <- get setBlogPostCategoryId
  case mEnt of
    Nothing -> pure False
    Just _ -> True <$ update setBlogPostCategoryId
      [ StoredBlogPostCategoryName =. setBlogPostCategoryName
      , StoredBlogPostCategoryPriority =. setBlogPostCategoryPriority
      , StoredBlogPostCategoryPermalink =. setBlogPostCategoryPermalink
      ]


-- * Post

getBlogPosts :: Permalink -- ^ Category
             -> ReaderT SqlBackend IO [BlogPostSynopsis]
getBlogPosts category = do
  mCat <- getBy (UniqueBlogPostCategory category)
  case mCat of
    Nothing -> pure []
    Just (Entity categoryId _) -> do
      ents <- selectList [StoredBlogPostCategory' ==. categoryId] []
      fmap catMaybes $ forM ents $ \(Entity _ (StoredBlogPost author timestamp headline permalink _ variant priority _)) -> do
        mAuthor <- get author
        case mAuthor of
          Nothing -> pure Nothing
          Just (StoredEditor _ name) -> pure $ Just BlogPostSynopsis
            { blogPostSynopsisAuthor = name
            , blogPostSynopsisTimestamp = timestamp
            , blogPostSynopsisHeadline = headline
            , blogPostSynopsisPermalink = permalink
            , blogPostSynopsisVariant = variant
            , blogPostSynopsisPriority = priority
            }

getBlogPost :: Permalink -- ^ Category
            -> Permalink -- ^ Post
            -> ReaderT SqlBackend IO (Maybe GetBlogPost)
getBlogPost category permalink = do
  mCat <- getBy (UniqueBlogPostCategory category)
  case mCat of
    Nothing -> pure Nothing
    Just (Entity categoryId (StoredBlogPostCategory categoryName _ _)) -> do
      mEnt <- getBy (UniqueBlogPost categoryId permalink)
      case mEnt of
        Nothing -> pure Nothing
        Just (Entity postId (StoredBlogPost author timestamp headline _ content variant priority _)) -> do
          mAuthor <- get author
          case mAuthor of
            Nothing -> pure Nothing
            Just (StoredEditor _ name) ->
              pure $ Just GetBlogPost
                { Blog.getBlogPostAuthor = name
                , Blog.getBlogPostTimestamp = timestamp
                , Blog.getBlogPostHeadline = headline
                , Blog.getBlogPostPermalink = permalink
                , Blog.getBlogPostContent = content
                , Blog.getBlogPostVariant = variant
                , Blog.getBlogPostPriority = priority
                , Blog.getBlogPostCategory = categoryName
                , Blog.getBlogPostId = postId
                }

newBlogPost :: AuthToken -> NewBlogPost -> SystemM (Maybe StoredBlogPostId)
newBlogPost authToken NewBlogPost{..} = do
  mAuthor <- verifyEditorhood authToken
  case mAuthor of
    Nothing -> pure Nothing
    Just author -> do
      liftDb $ do
        mExisting <- getBy (UniqueBlogPost newBlogPostCategory newBlogPostPermalink)
        case mExisting of
          Just _ -> pure Nothing
          Nothing -> do
            now <- liftIO getCurrentTime
            let insertBlogPost = insert $ StoredBlogPost
                  author
                  now
                  newBlogPostHeadline
                  newBlogPostPermalink
                  newBlogPostContent
                  newBlogPostVariant
                  newBlogPostPriority
                  newBlogPostCategory
            if newBlogPostPrimary
              then do
                mPrimary <- getBy (UniquePrimaryBlogPost newBlogPostCategory)
                case mPrimary of
                  Just _ -> pure Nothing
                  Nothing -> do
                    postId <- insertBlogPost
                    insert_ (StoredBlogPostPrimary postId newBlogPostCategory)
                    pure (Just postId)
              else Just <$> insertBlogPost


setBlogPost :: AuthToken -> SetBlogPost -> SystemM Bool
setBlogPost authToken SetBlogPost{..} = do
  mAuthor <- verifyEditorhood authToken
  case mAuthor of
    Nothing -> pure False
    Just author -> do
      liftDb $ do
        mPost <- get setBlogPostId
        case mPost of
          Nothing -> pure False
          Just (StoredBlogPost _ _ _ _ _ _ _ category) -> do
            let updatePost = do
                  mPrimary <- getBy (UniqueBlogPost category setBlogPostPermalink)
                  case mPrimary of
                    Just _ -> pure False
                    Nothing -> do
                      update setBlogPostId
                        [ StoredBlogPostAuthor =. author
                        , StoredBlogPostHeadline =. setBlogPostHeadline
                        , StoredBlogPostPermalink =. setBlogPostPermalink
                        , StoredBlogPostContent =. setBlogPostContent
                        , StoredBlogPostVariant =. setBlogPostVariant
                        , StoredBlogPostPriority =. setBlogPostPriority
                        ]
                      pure True
            if not setBlogPostPrimary
              then do
                deleteWhere
                  [ StoredBlogPostPrimaryCategory ==. category
                  , StoredBlogPostPrimaryPost ==. setBlogPostId
                  ]
                updatePost
              else do
                mPrimary <- getBy (UniquePrimaryBlogPost category)
                case mPrimary of
                  Just (Entity _ (StoredBlogPostPrimary primaryPost _))
                    | primaryPost /= setBlogPostId -> pure False
                    | otherwise -> updatePost
                  Nothing -> do
                    insert_ (StoredBlogPostPrimary setBlogPostId category)
                    updatePost


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


verifyEditorhood :: AuthToken -> SystemM (Maybe StoredEditorId)
verifyEditorhood authToken = do
  mUserId <- getUserId authToken
  case mUserId of
    Nothing -> pure Nothing
    Just userId -> do
      liftDb $ do
        isEditor <- hasRole userId Editor
        if not isEditor
          then pure Nothing
          else do
            mEnt <- getBy (UniqueEditor userId)
            case mEnt of
              Nothing -> pure Nothing
              Just (Entity editorId _) -> pure (Just editorId)
