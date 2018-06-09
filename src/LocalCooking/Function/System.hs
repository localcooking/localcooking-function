{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  , RecordWildCards
  , GeneralizedNewtypeDeriving
  , DeriveFunctor
  , UndecidableInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

module LocalCooking.Function.System
  ( SystemM, execSystemM
  , SystemEnv (..)
  , NewSystemEnvArgs (..)
  , Managers (..)
  , TokenContexts (..)
  , Keys (..)
  , getUserId
  , guardRole
  , getSystemEnv
  ) where

import LocalCooking.Function.System.AccessToken (AccessTokenContext, newAccessTokenContext, expireThread, lookupAccess)
import LocalCooking.Function.System.Review (ReviewAccumulator, newReviewAccumulator, calculateThread)
import LocalCooking.Common.AccessToken.Email (EmailToken)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.User.Password (HashedPassword)
import LocalCooking.Common.User.Role (UserRole)
import LocalCooking.Database.Schema (migrateAll)
import LocalCooking.Database.Schema.User (StoredUserId)
import LocalCooking.Database.Query.Salt (getPasswordSalt)
import LocalCooking.Database.Query.Semantics.Admin (hasRole)
import Facebook.Types (FacebookAppCredentials)
import Google.Keys (GoogleCredentials)
import SparkPost.Keys (SparkPostCredentials)

import Data.Aeson (FromJSON (..), (.:), Value (Object))
import Data.Aeson.Types (typeMismatch)
import Data.Default (Default (..))
import Data.Pool (destroyAllResources)
import Data.Monoid ((<>))
import Data.Time.Clock (secondsToDiffTime)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.URI (URI)
import Data.Functor.Identity (Identity)
import Data.Functor.Compose (Compose)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Catch (MonadCatch, MonadThrow, MonadMask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO (..), UnliftIO (..))
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Monad.Trans.Control.Aligned as Aligned
import Control.Monad.Trans.Unlift (MonadBaseUnlift)
import Control.Exception (bracket)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM (STM, atomically)
import Database.Persist.Sql (ConnectionPool)
import Database.Persist.Postgresql (createPostgresqlPool)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import System.IO.Unsafe (unsafePerformIO)




newtype SystemM a = SystemM
  { getSystemM :: ReaderT SystemEnv IO a
  } deriving ( Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadBaseControl IO
             , MonadBaseUnlift IO, MonadCatch, MonadThrow, MonadMask)

instance MonadUnliftIO SystemM where
  askUnliftIO = SystemM $ do
    UnliftIO f <- askUnliftIO
    pure $ UnliftIO $ f . getSystemM
  withRunInIO runner = SystemM $ withRunInIO $ \fromM -> runner (fromM . getSystemM)

instance Aligned.MonadBaseControl IO SystemM (Compose Identity Identity) where
  liftBaseWith f = SystemM $ Aligned.liftBaseWith $ \runInBase ->
    f (runInBase . getSystemM)
  restoreM = SystemM . Aligned.restoreM



getSystemEnv :: SystemM SystemEnv
getSystemEnv = SystemM ask


execSystemM :: NewSystemEnvArgs
            -> SystemM a -> IO a
execSystemM args (SystemM x) = bracket build release $ \(env,_,_,_) -> do
  runReaderT x env
  where
    build = do
      env@SystemEnv
        { systemEnvTokenContexts
        , systemEnvReviews
        , systemEnvDatabase
        } <- newSystemEnv args
      case systemEnvTokenContexts of
        TokenContexts auth email -> do
          let second = fromRational $ toRational $ secondsToDiffTime 1
          thread1 <- async $ expireThread (3600 * second) auth
          thread2 <- async $ expireThread (3600 * second) email
          thread3 <- async $ calculateThread systemEnvDatabase systemEnvReviews
          pure (env,thread1,thread2,thread3)

    release (env,thread1,thread2,thread3) = do
      releaseSystemEnv env
      cancel thread1
      cancel thread2
      cancel thread3



data SystemEnv = SystemEnv
  { systemEnvDatabase      :: ConnectionPool
  , systemEnvKeys          :: Keys
  , systemEnvManagers      :: Managers
  , systemEnvSalt          :: HashedPassword
  , systemEnvTokenContexts :: TokenContexts
  , systemEnvReviews       :: ReviewAccumulator
  , systemEnvFBRedirect    :: URI
  }


data NewSystemEnvArgs = NewSystemEnvArgs
  { dbHost           :: Text
  , dbPort           :: Int
  , dbUser           :: Text
  , dbPassword       :: Text
  , dbName           :: Text
  , keys             :: Keys
  , facebookRedirect :: URI
  }


newSystemEnv :: NewSystemEnvArgs -> IO SystemEnv
newSystemEnv NewSystemEnvArgs{..} = do
  let connStr = T.encodeUtf8 $ T.unwords
        [ "host=" <> dbHost
        , "port=" <> T.pack (show dbPort)
        , "user=" <> dbUser
        , "password=" <> dbPassword
        , "dbname=" <> dbName
        ]
  systemEnvDatabase <- runStderrLoggingT (createPostgresqlPool connStr 10)
  migrateAll systemEnvDatabase
  systemEnvManagers <- defManagers
  systemEnvTokenContexts <- atomically defTokenContexts
  systemEnvSalt <- getPasswordSalt systemEnvDatabase
  systemEnvReviews <- newReviewAccumulator

  pure SystemEnv
    { systemEnvDatabase
    , systemEnvKeys = keys
    , systemEnvManagers
    , systemEnvSalt
    , systemEnvTokenContexts
    , systemEnvReviews
    , systemEnvFBRedirect = facebookRedirect
    }



releaseSystemEnv :: SystemEnv -> IO ()
releaseSystemEnv SystemEnv{systemEnvDatabase} =
  destroyAllResources systemEnvDatabase




-- * Support Data Types

-- | HTTP managers
data Managers = Managers
  { managersFacebook  :: Manager
  , managersReCaptcha :: Manager
  , managersSparkPost :: Manager
  }

instance Default Managers where
  def = unsafePerformIO defManagers

defManagers :: IO Managers
defManagers = do
  managersFacebook <- newTlsManager -- FIXME could bug out from facebook booting us
  managersReCaptcha <- newTlsManager
  managersSparkPost <- newTlsManager
  pure Managers
    { managersFacebook
    , managersReCaptcha
    , managersSparkPost
    }

-- | Access token contexts, for expiring references
data TokenContexts = TokenContexts
  { tokenContextAuth :: AccessTokenContext AuthToken StoredUserId
  , tokenContextEmail :: AccessTokenContext EmailToken StoredUserId
  }

instance Default TokenContexts where
  def = unsafePerformIO (atomically defTokenContexts)

defTokenContexts :: STM TokenContexts
defTokenContexts = do
  tokenContextAuth <- newAccessTokenContext
  tokenContextEmail <- newAccessTokenContext
  pure TokenContexts
    { tokenContextAuth
    , tokenContextEmail
    }


-- | Data stored in @~/.localcooking/secret@
data Keys = Keys
  { keysFacebook :: FacebookAppCredentials
  , keysGoogle :: GoogleCredentials
  , keysSparkPost :: SparkPostCredentials
  }


instance FromJSON Keys where
  parseJSON (Object o) = do
    keysFacebook <- o .: "facebook"
    keysGoogle <- o .: "google"
    keysSparkPost <- o .: "sparkPost"
    pure Keys{keysFacebook,keysGoogle,keysSparkPost}
  parseJSON x = typeMismatch "Keys" x



-- * Utils


getUserId :: AuthToken -> SystemM (Maybe StoredUserId)
getUserId authToken = do
  SystemEnv{systemEnvTokenContexts} <- getSystemEnv
  case systemEnvTokenContexts of
    TokenContexts{tokenContextAuth} ->
      liftIO (lookupAccess tokenContextAuth authToken)


guardRole :: StoredUserId -> UserRole -> SystemM Bool
guardRole userId r = do
  SystemEnv{systemEnvDatabase} <- getSystemEnv
  liftIO (hasRole systemEnvDatabase userId r)
