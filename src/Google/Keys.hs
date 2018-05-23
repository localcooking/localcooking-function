{-# LANGUAGE
    OverloadedStrings
  , OverloadedLists
  , RecordWildCards
  , GeneralizedNewtypeDeriving
  #-}

{-|

Module: Google.Keys
Copyright: (c) 2018 Local Cooking Inc.
License: Proprietary
Maintainer: athan.clark@localcooking.com
Portability: GHC

-}

module Google.Keys where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (Object), (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
import Data.URI (URI (..))
import Data.URI.Auth (URIAuth (..))
import Data.URI.Auth.Host (URIAuthHost (Host))
import qualified Data.Strict.Maybe as Strict
import Data.Strict.Tuple (Pair (..))


data GoogleCredentials = GoogleCredentials
  { googleAnalytics :: GoogleAnalyticsGTag
  , googleReCaptcha :: ReCaptchaSiteKey
  , googleReCaptchaSecret :: ReCaptchaSecret
  } deriving (Eq, Show)


instance FromJSON GoogleCredentials where
  parseJSON json = case json of
    Object o -> GoogleCredentials <$> o .: "analytics"
                                  <*> o .: "reCaptcha"
                                  <*> o .: "reCaptchaSecret"
    _ -> fail
    where
      fail = typeMismatch "GoogleCredentials" json


-- * Analytics

-- | API Key
newtype GoogleAnalyticsGTag = GoogleAnalyticsGTag
  { googleAnalyticsGTag :: Text
  } deriving (Eq, Show, FromJSON)


-- | Generates a link from a Google GTag.
googleAnalyticsGTagToURI :: GoogleAnalyticsGTag -> URI
googleAnalyticsGTagToURI GoogleAnalyticsGTag{..} =
  URI
    (Strict.Just "https")
    True
    (URIAuth Strict.Nothing (Host ["www","googletagmanager"] "com") Strict.Nothing)
    ["gtag", "js"]
    ["id" :!: Strict.Just googleAnalyticsGTag]
    Strict.Nothing


-- * ReCaptcha


-- | Public ReCaptcha API Key
newtype ReCaptchaSiteKey = ReCaptchaSiteKey
  { getReCaptchaSiteKey :: Text
  } deriving (Eq, Show, FromJSON, ToJSON)

-- | Private ReCaptcha API Key
newtype ReCaptchaSecret = ReCaptchaSecret
  { getReCaptchaSecret :: Text
  } deriving (Eq, Show, FromJSON, ToJSON)



-- | ReCaptcha javascript link
googleReCaptchaAssetURI :: URI
googleReCaptchaAssetURI =
  URI
    (Strict.Just "https")
    True
    (URIAuth Strict.Nothing (Host ["www","google"] "com") Strict.Nothing)
    ["recaptcha", "api.js"]
    []
    Strict.Nothing


-- | ReCaptcha Response Code
newtype ReCaptchaResponse = ReCaptchaResponse
  { getReCaptchaResponse :: Text
  } deriving (Eq, Show, FromJSON, ToJSON)


-- | ReCaptcha verification link
googleReCaptchaVerifyURI :: URI
googleReCaptchaVerifyURI =
  URI
    (Strict.Just "https")
    True
    (URIAuth Strict.Nothing (Host ["www","google"] "com") Strict.Nothing)
    ["recaptcha", "api", "siteverify"]
    []
    Strict.Nothing


-- | ReCaptcha verification response
data ReCaptchaVerifyResponse = ReCaptchaVerifyResponse Bool

instance FromJSON ReCaptchaVerifyResponse where
  parseJSON (Object o) = ReCaptchaVerifyResponse <$> o .: "success"
  parseJSON json = typeMismatch "ReCaptchaVerifyResponse" json
