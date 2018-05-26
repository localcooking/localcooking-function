{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  , RecordWildCards
  , GeneralizedNewtypeDeriving
  , DeriveFunctor
  #-}

module LocalCooking.Function.System
  ( AppM, execAppM
  , SystemEnv (..)
  , NewSystemEnvArgs (..)
  , Managers (..)
  , TokenContexts (..)
  , Keys (..)
  , getUserId
  , guardRole
  ) where

import LocalCooking.Function.System.AccessToken (AccessTokenContext, newAccessTokenContext, expireThread, lookupAccess)
import LocalCooking.Function.System.Review (ReviewAccumulator, newReviewAccumulator, calculateThread)
import LocalCooking.Common.AccessToken.Email (EmailToken)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.User.Password (HashedPassword)
import LocalCooking.Common.User.Role (UserRole)
import LocalCooking.Database.Schema.User (StoredUserId)
import LocalCooking.Database.Query.Salt (getPasswordSalt)
import LocalCooking.Database.Query.Semantics.Admin (hasRole)
import Facebook.App (FacebookAppCredentials)
import Google.Keys (GoogleCredentials)
import SparkPost.Keys (SparkPostCredentials)

import Data.Aeson (FromJSON (..), (.:), Value (Object))
import Data.Aeson.Types (typeMismatch)
import Data.Default (Default (..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Pool (destroyAllResources)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock (secondsToDiffTime)
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.UTF8 as BS8
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Exception (bracket)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM (STM, atomically, TVar, newTVarIO)
import Database.Persist.Sql (ConnectionPool)
import Database.Persist.Postgresql (createPostgresqlPool)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import System.IO.Unsafe (unsafePerformIO)




type AppM = ReaderT SystemEnv IO

execAppM :: NewSystemEnvArgs -> AppM a -> IO a
execAppM args x = bracket build release $ \(env,_,_,_) -> runReaderT x env
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
  -- , systemEnvPendingEmail  :: TVar (HashMap EmailToken StoredUserId)
  , systemEnvReviews       :: ReviewAccumulator
  }


data NewSystemEnvArgs = NewSystemEnvArgs
  { dbHost :: Text
  , dbPort :: Int
  , dbUser :: Text
  , dbPassword :: Text
  , dbName :: Text
  , keys :: Keys
  }


newSystemEnv :: NewSystemEnvArgs -> IO SystemEnv
newSystemEnv NewSystemEnvArgs{..} = do
  let connStr = "host=" <> T.encodeUtf8 dbHost
              <> " port=" <> BS8.fromString (show dbPort)
              <> " user=" <> T.encodeUtf8 dbUser
              <> " password=" <> T.encodeUtf8 dbPassword
              <> " dbname=" <> T.encodeUtf8 dbName
  systemEnvDatabase <- runStderrLoggingT (createPostgresqlPool connStr 10)
  systemEnvManagers <- defManagers
  systemEnvTokenContexts <- atomically defTokenContexts
  systemEnvSalt <- getPasswordSalt systemEnvDatabase
  -- systemEnvPendingEmail <- newTVarIO HashMap.empty
  systemEnvReviews <- newReviewAccumulator

  pure SystemEnv
    { systemEnvDatabase
    , systemEnvKeys = keys
    , systemEnvManagers
    , systemEnvSalt
    , systemEnvTokenContexts
    -- , systemEnvPendingEmail
    , systemEnvReviews
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


getUserId :: AuthToken -> AppM (Maybe StoredUserId)
getUserId authToken = do
  SystemEnv{systemEnvTokenContexts} <- ask
  case systemEnvTokenContexts of
    TokenContexts{tokenContextAuth} ->
      liftIO (lookupAccess tokenContextAuth authToken)


guardRole :: StoredUserId -> UserRole -> AppM Bool
guardRole userId r = do
  SystemEnv{systemEnvDatabase} <- ask
  liftIO (hasRole systemEnvDatabase userId r)
