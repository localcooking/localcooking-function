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
  ) where

import LocalCooking.Function.System.AccessToken (AccessTokenContext, newAccessTokenContext)
import LocalCooking.Common.AccessToken.Email (EmailToken)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.User.Password (HashedPassword)
import LocalCooking.Database.Schema.User (StoredUserId)
import LocalCooking.Database.Query.Salt (getPasswordSalt)
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
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.UTF8 as BS8
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Exception (bracket)
import Control.Concurrent.STM (STM, atomically, TVar, newTVarIO)
import Database.Persist.Sql (ConnectionPool)
import Database.Persist.Postgresql (createPostgresqlPool)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import System.IO.Unsafe (unsafePerformIO)




type AppM = ReaderT SystemEnv IO

execAppM :: NewSystemEnvArgs -> AppM a -> IO a
execAppM args x = bracket (newSystemEnv args) releaseSystemEnv (runReaderT x)



data SystemEnv = SystemEnv
  { systemEnvDatabase      :: ConnectionPool
  , systemEnvKeys          :: Keys
  , systemEnvManagers      :: Managers
  , systemEnvSalt          :: HashedPassword
  , systemEnvTokenContexts :: TokenContexts
  , systemEnvPendingEmail  :: TVar (HashMap EmailToken StoredUserId)
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
  systemEnvPendingEmail <- newTVarIO HashMap.empty

  pure SystemEnv
    { systemEnvDatabase
    , systemEnvKeys = keys
    , systemEnvManagers
    , systemEnvSalt
    , systemEnvTokenContexts
    , systemEnvPendingEmail
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
