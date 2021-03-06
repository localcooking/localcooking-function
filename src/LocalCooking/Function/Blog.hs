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
  (SystemM, SystemEnv (..), getSystemEnv, liftDb)
import LocalCooking.Function.User (getUserId, verifyRole)
import LocalCooking.Semantics.Blog
  ( GetBlogPost (GetBlogPost), NewBlogPost (NewBlogPost), SetBlogPost (SetBlogPost)
  , BlogPostSynopsis (..), BlogPostCategorySynopsis (..), GetBlogPostCategory (..)
  , SetBlogPostCategory (..), NewBlogPostCategory (..)
  , BlogPostCategoryExists (..), BlogPostCategoryUnique (..), BlogPostExists (..)
  , BlogPostUnique (..), BlogPostPrimary (..)
  )
import qualified LocalCooking.Semantics.Blog as Blog
import LocalCooking.Semantics.User (HasRole (..), UserExists (..))
import LocalCooking.Semantics.Content (EditorExists (..))
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.User.Role (UserRole (Editor))
import LocalCooking.Common.Blog (BlogPostVariant)
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
    , StoredBlogPostPrimaryPost, StoredBlogPostAuthor
    , StoredBlogPostCategoryVariant
    )
  )

import Data.Maybe (catMaybes)
import Data.Time (getCurrentTime)
import Data.Text.Permalink (Permalink)
import Data.Aeson.JSONTuple (JSONTuple (..))
import Data.Aeson.JSONUnit (JSONUnit (..))
import Control.Monad (forM)
import Control.Monad.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.Persist (Entity (..), (==.), (=.), SelectOpt (Asc))
import Database.Persist.Sql (SqlBackend, runSqlPool)
import Database.Persist.Class
  (selectList, selectFirst, getBy, insert, insert_, update, get, deleteWhere)



-- * Category


getBlogPostCategories :: BlogPostVariant -> ReaderT SqlBackend IO [BlogPostCategorySynopsis]
getBlogPostCategories variant = do
  xs <- selectList
    [StoredBlogPostCategoryVariant ==. variant]
    [Asc StoredBlogPostCategoryPriority]
  pure $ (\(Entity _ (StoredBlogPostCategory name priority permalink _)) ->
            BlogPostCategorySynopsis name permalink priority) <$> xs

getBlogPostCategory :: BlogPostVariant
                    -> Permalink
                    -> ReaderT SqlBackend IO
                       (BlogPostCategoryUnique GetBlogPostCategory)
getBlogPostCategory variant permalink = do
  mEnt <- getBy (UniqueBlogPostCategory variant permalink)
  case mEnt of
    Nothing -> pure BlogPostCategoryNotUnique
    Just (Entity categoryId (StoredBlogPostCategory name _ _ _)) -> fmap BlogPostCategoryUnique $ do
      primary <- do
        mPrimary <- selectFirst [StoredBlogPostPrimaryCategory ==. categoryId] []
        case mPrimary of
          Nothing -> pure Nothing
          Just (Entity _ (StoredBlogPostPrimary primaryPost _)) -> getBlogPostSynopsisById primaryPost
      posts <- do
        xs <- selectList [StoredBlogPostCategory' ==. categoryId] [Asc StoredBlogPostPriority]
        fmap catMaybes $ forM xs $ \(Entity postId _) -> getBlogPostSynopsisById postId
      pure $ GetBlogPostCategory name permalink primary posts variant categoryId


newBlogPostCategory :: AuthToken
                    -> NewBlogPostCategory
                    -> SystemM
                       ( UserExists
                         ( HasRole
                           ( BlogPostCategoryUnique StoredBlogPostCategoryId)))
newBlogPostCategory authToken NewBlogPostCategory{..} =
  verifyRole Editor authToken $ \userId -> liftDb $ do
    mEnt <- getBy (UniqueBlogPostCategory newBlogPostCategoryVariant newBlogPostCategoryPermalink)
    case mEnt of
      Just _ -> pure BlogPostCategoryNotUnique
      Nothing -> fmap BlogPostCategoryUnique $ insert $ StoredBlogPostCategory
        newBlogPostCategoryName
        newBlogPostCategoryPriority
        newBlogPostCategoryPermalink
        newBlogPostCategoryVariant


setBlogPostCategory :: AuthToken
                    -> SetBlogPostCategory
                    -> SystemM
                       ( UserExists
                         ( HasRole
                           ( BlogPostCategoryExists
                             ( BlogPostCategoryUnique JSONUnit))))
setBlogPostCategory authToken SetBlogPostCategory{..} = do
  verifyRole Editor authToken $ \userId -> liftDb $ do
    mEnt <- get setBlogPostCategoryId
    case mEnt of
      Nothing -> pure BlogPostCategoryDoesntExist
      Just _ -> fmap BlogPostCategoryExists $ do
        mExisting <- getBy (UniqueBlogPostCategory setBlogPostCategoryVariant setBlogPostCategoryPermalink)
        case mExisting of
          Just _ -> pure BlogPostCategoryNotUnique
          Nothing -> do
            BlogPostCategoryUnique JSONUnit <$ update setBlogPostCategoryId
              [ StoredBlogPostCategoryName =. setBlogPostCategoryName
              , StoredBlogPostCategoryPriority =. setBlogPostCategoryPriority
              , StoredBlogPostCategoryPermalink =. setBlogPostCategoryPermalink
              , StoredBlogPostCategoryVariant =. setBlogPostCategoryVariant
              ]


-- * Post

-- TODO order by priority
getBlogPosts :: BlogPostVariant -> Permalink -- ^ Category
             -> ReaderT SqlBackend IO (BlogPostCategoryUnique [EditorExists BlogPostSynopsis])
getBlogPosts variant category = do
  mCat <- getBy (UniqueBlogPostCategory variant category)
  case mCat of
    Nothing -> pure BlogPostCategoryNotUnique
    Just (Entity categoryId _) -> fmap BlogPostCategoryUnique $ do
      ents <- selectList [StoredBlogPostCategory' ==. categoryId] []
      forM ents $ \(Entity _ (StoredBlogPost author timestamp headline permalink _ priority _)) -> do
        mAuthor <- get author
        case mAuthor of
          Nothing -> pure EditorDoesntExist
          Just (StoredEditor _ name) -> pure $ EditorExists BlogPostSynopsis
            { blogPostSynopsisAuthor = name
            , blogPostSynopsisTimestamp = timestamp
            , blogPostSynopsisHeadline = headline
            , blogPostSynopsisPermalink = permalink
            , blogPostSynopsisPriority = priority
            }

getBlogPost :: JSONTuple BlogPostVariant (JSONTuple Permalink Permalink)
            -> ReaderT SqlBackend IO
               ( BlogPostCategoryUnique
                 ( BlogPostUnique
                   ( EditorExists GetBlogPost)))
getBlogPost (JSONTuple variant (JSONTuple category permalink)) = do
  mCat <- getBy (UniqueBlogPostCategory variant category)
  case mCat of
    Nothing -> pure BlogPostCategoryNotUnique
    Just (Entity categoryId (StoredBlogPostCategory categoryName _ _ _)) -> fmap BlogPostCategoryUnique $ do
      mEnt <- getBy (UniqueBlogPost categoryId permalink)
      case mEnt of
        Nothing -> pure BlogPostNotUnique
        Just (Entity postId (StoredBlogPost author timestamp headline _ content priority _)) -> fmap BlogPostUnique $ do
          mAuthor <- get author
          case mAuthor of
            Nothing -> pure EditorDoesntExist
            Just (StoredEditor _ name) ->
              pure $ EditorExists GetBlogPost
                { Blog.getBlogPostAuthor = name
                , Blog.getBlogPostTimestamp = timestamp
                , Blog.getBlogPostHeadline = headline
                , Blog.getBlogPostPermalink = permalink
                , Blog.getBlogPostContent = content
                , Blog.getBlogPostPriority = priority
                , Blog.getBlogPostCategory = categoryName
                , Blog.getBlogPostId = postId
                }

newBlogPost :: AuthToken
            -> NewBlogPost
            -> SystemM
               ( UserExists
                 ( HasRole
                   ( EditorExists
                     ( BlogPostUnique
                       ( BlogPostPrimary StoredBlogPostId)))))
newBlogPost authToken NewBlogPost{..} = do
  verifyRole Editor authToken $ \userId -> liftDb $ do
    mAuthor <- getBy (UniqueEditor userId)
    case mAuthor of
      Nothing -> pure EditorDoesntExist
      Just (Entity author _) -> fmap EditorExists $ do
        mExisting <- getBy (UniqueBlogPost newBlogPostCategory newBlogPostPermalink)
        case mExisting of
          Just _ -> pure BlogPostNotUnique
          Nothing -> fmap BlogPostUnique $ do
            now <- liftIO getCurrentTime
            let insertBlogPost = insert $ StoredBlogPost
                  author
                  now
                  newBlogPostHeadline
                  newBlogPostPermalink
                  newBlogPostContent
                  newBlogPostPriority
                  newBlogPostCategory
            if newBlogPostPrimary
              then do
                mPrimary <- getBy (UniquePrimaryBlogPost newBlogPostCategory)
                case mPrimary of
                  Just _ -> pure BlogPostNotPrimary
                  Nothing -> do
                    postId <- insertBlogPost
                    insert_ (StoredBlogPostPrimary postId newBlogPostCategory)
                    pure (BlogPostPrimary postId)
              else BlogPostPrimary <$> insertBlogPost


setBlogPost :: AuthToken
            -> SetBlogPost
            -> SystemM
               ( UserExists
                 ( HasRole
                   ( EditorExists
                     ( BlogPostExists
                       ( BlogPostPrimary
                         ( BlogPostUnique JSONUnit))))))
setBlogPost authToken SetBlogPost{..} = do
  verifyRole Editor authToken $ \userId -> liftDb $ do
    mAuthor <- getBy (UniqueEditor userId)
    case mAuthor of
      Nothing -> pure EditorDoesntExist
      Just (Entity author _) -> fmap EditorExists $ do
        mPost <- get setBlogPostId
        case mPost of
          Nothing -> pure BlogPostDoesntExist
          Just (StoredBlogPost _ _ _ _ _ _ category) -> fmap BlogPostExists $ do
            let updatePost = do
                  mPost <- getBy (UniqueBlogPost category setBlogPostPermalink)
                  case mPost of
                    Just _ -> pure BlogPostNotUnique
                    Nothing -> do
                      update setBlogPostId
                        [ StoredBlogPostAuthor =. author
                        , StoredBlogPostHeadline =. setBlogPostHeadline
                        , StoredBlogPostPermalink =. setBlogPostPermalink
                        , StoredBlogPostContent =. setBlogPostContent
                        , StoredBlogPostPriority =. setBlogPostPriority
                        ]
                      pure (BlogPostUnique JSONUnit)
            if not setBlogPostPrimary
              then do
                deleteWhere
                  [ StoredBlogPostPrimaryCategory ==. category
                  , StoredBlogPostPrimaryPost ==. setBlogPostId
                  ]
                BlogPostPrimary <$> updatePost
              else do
                mPrimary <- getBy (UniquePrimaryBlogPost category)
                case mPrimary of
                  Just (Entity _ (StoredBlogPostPrimary primaryPost _))
                    | primaryPost /= setBlogPostId -> pure BlogPostNotPrimary
                    | otherwise -> BlogPostPrimary <$> updatePost
                  Nothing -> do
                    insert_ (StoredBlogPostPrimary setBlogPostId category)
                    BlogPostPrimary <$> updatePost


-- * Unexposed

getBlogPostSynopsisById :: StoredBlogPostId -> ReaderT SqlBackend IO (Maybe BlogPostSynopsis)
getBlogPostSynopsisById k = do
  mPost <- get k
  case mPost of
    Nothing -> pure Nothing
    Just (StoredBlogPost author timestamp headline permalink _ priority _) -> do
      mEditor <- get author
      case mEditor of
        Nothing -> pure Nothing
        Just (StoredEditor _ name) ->
          pure $ Just $
            BlogPostSynopsis name timestamp headline permalink priority
