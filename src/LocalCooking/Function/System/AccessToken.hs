{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleContexts
  , RecordWildCards
  , NamedFieldPuns
  , RankNTypes
  , FunctionalDependencies
  , ScopedTypeVariables
  , OverloadedStrings
  #-}

module LocalCooking.Function.System.AccessToken where

import LocalCooking.Common.AccessToken (AccessToken, genAccessToken)

import Data.Hashable (Hashable)
import Data.Time (NominalDiffTime)
import Data.TimeMap (TimeMap, newTimeMap)
import qualified Data.TimeMap as TimeMap
import Control.Monad (forM_, forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TMapMVar.Hash (TMapMVar, newTMapMVar)
import qualified Control.Concurrent.STM.TMapMVar.Hash as TMapMVar
import Control.Newtype (Newtype (pack))



-- * Context

-- | Global context for an access token - expiring mapping with concurrent locks
data AccessTokenContext k a = AccessTokenContext
  { accessTokenContextSubject :: TimeMap k a
  , accessTokenContextExpire  :: TMapMVar k () -- ^ Expiration lock
  }



newAccessTokenContext :: Newtype k AccessToken => STM (AccessTokenContext k a)
newAccessTokenContext = AccessTokenContext <$> newTimeMap <*> newTMapMVar


-- * Utilities

revokeAccess :: Hashable k
             => Eq k
             => AccessTokenContext k a
             -> k
             -> STM ()
revokeAccess AccessTokenContext{..} accessToken = do
  TimeMap.delete accessToken accessTokenContextSubject
  TMapMVar.insert accessTokenContextExpire accessToken ()


insertAccess :: Hashable k
             => Eq k
             => Newtype k AccessToken
             => AccessTokenContext k a
             -> a
             -> IO k
insertAccess AccessTokenContext{accessTokenContextSubject} x = do
  accessToken <- pack <$> genAccessToken
  TimeMap.insert accessToken x accessTokenContextSubject
  pure accessToken



lookupAccess :: Hashable k
             => Eq k
             => AccessTokenContext k a
             -> k
             -> IO (Maybe a)
lookupAccess AccessTokenContext{accessTokenContextSubject} accessToken = do
  mSubj <- atomically (TimeMap.lookup accessToken accessTokenContextSubject)
  TimeMap.touch accessToken accessTokenContextSubject
  pure mSubj



-- * Global Functions

-- | Independently forked thread that drains the timemap and invokes the concurrency locks when
-- tokens expire.
expireThread  :: Hashable k
              => Eq k
              => NominalDiffTime
              -> AccessTokenContext k a
              -> IO ()
expireThread expiration AccessTokenContext{..} = forever $ do
  xs <- TimeMap.takeFromNow expiration accessTokenContextSubject
  forM_ xs $ \(authToken,_) ->
    atomically (TMapMVar.insert accessTokenContextExpire authToken ())
  threadDelay $
    let second = 10 ^ 6
        minute = second * 60
    in  minute
