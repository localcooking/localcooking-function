{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
  #-}

module SparkPost.Keys where

import LocalCooking.Common.AccessToken.Email (EmailToken)

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String, Object), (.:), object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (typeMismatch)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.EmailAddress (EmailAddress)
import Network.HTTP.Client (Request, parseRequest, requestHeaders, requestBody, RequestBody (RequestBodyLBS), method)



data SparkPostCredentials = SparkPostCredentials
  { sparkPostKey :: SparkPostKey
  } deriving (Show, Eq)

instance FromJSON SparkPostCredentials where
  parseJSON json = case json of
    Object o -> SparkPostCredentials <$> o .: "outgoing"
    _ -> fail'
    where
      fail' = typeMismatch "SparkPostCredentials" json


newtype SparkPostKey = SparkPostKey
  { getSparkPostKey :: Text
  } deriving (Eq, ToJSON, FromJSON)

instance Show SparkPostKey where
  show (SparkPostKey x) = T.unpack x

showSparkPostKey :: SparkPostKey -> ByteString
showSparkPostKey (SparkPostKey x) = T.encodeUtf8 x



confirmEmailRequest :: SparkPostKey -> EmailAddress -> EmailToken -> IO Request
confirmEmailRequest key email emailToken = do
  req <- parseRequest "https://api.sparkpost.com/api/v1/transmissions"
  let req' = req
        { requestHeaders =
          [ ("Authorization", showSparkPostKey key)
          , ("Content-Type", "application/json")
          , ("Accept", "application/json")
          ]
        , method = "POST"
        , requestBody = RequestBodyLBS (Aeson.encode body)
        }
      body = object
        [ "recipients" .=
          [ object
            [ "address" .= email
            , "substitution_data" .= object
              [ "emailCode" .= emailToken
              ]
            ]
          ]
        , "content" .= object
          [ "template_id" .= String "confirm-registration"
          ]
        ]
  pure req'
