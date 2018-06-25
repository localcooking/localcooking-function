{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module LocalCooking.Function.System.Search where

import LocalCooking.Common.Tag.Meal (MealTag (..))
import LocalCooking.Common.Tag.Chef (ChefTag (..))
import LocalCooking.Common.Tag (Tag (..))
import LocalCooking.Database.Schema (StoredMealTag (..), StoredChefTag (..))

import Data.Default (def)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.XML.Types as XML
import Text.XML.Stream.Render (renderBuilder)
import Data.Binary.Builder (Builder)
import Data.Conduit (ConduitT, (.|), yield, ($$))
import qualified Data.Conduit.List as CL
import Control.Monad (mapM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)

import Network.Wai (StreamingBody)
import Database.Persist (Entity (..), Key)
import Database.Persist.Class (selectSource)
import Database.Persist.Sql (SqlBackend, ConnectionPool, runSqlPool)



-- | Response handler for populating sphinx
sphinxDocumentHTTPStream :: ConduitT () XML.Event (ReaderT SqlBackend (ResourceT IO)) ()
                         -> ConnectionPool -> StreamingBody
sphinxDocumentHTTPStream doc backend chunk flush =
  runResourceT (runSqlPool (stream $$ runner) backend)
  where
    runner :: ConduitT Builder o (ReaderT SqlBackend (ResourceT IO)) ()
    runner = CL.foldMapM (\x -> liftIO $ chunk x >> flush)
    stream :: ConduitT () Builder (ReaderT SqlBackend (ResourceT IO)) ()
    stream = doc .| renderBuilder def


-- TODO different documents for other tags, compiled search techniques

mealTagsDocument :: ConduitT () XML.Event (ReaderT SqlBackend (ResourceT IO)) ()
mealTagsDocument = do
  mapM_ yield startEvents
  selectSource [] []
    .| CL.concatMap
         ( extractEntityToXMLEvent
           (\(StoredMealTag (MealTag (Tag x))) -> x)
         )
  mapM_ yield endEvents


chefTagsDocument :: ConduitT () XML.Event (ReaderT SqlBackend (ResourceT IO)) ()
chefTagsDocument = do
  mapM_ yield startEvents
  selectSource [] []
    .| CL.concatMap
         ( extractEntityToXMLEvent
           (\(StoredChefTag (ChefTag (Tag x))) -> x)
         )
  mapM_ yield endEvents



extractEntityToXMLEvent :: Show (Key a) => (a -> Text) -> Entity a -> [XML.Event]
extractEntityToXMLEvent get (Entity idx x) =
  [ XML.EventBeginElement document [("id", [XML.ContentText $ T.pack $ show idx])]
  , XML.EventBeginElement content []
  , XML.EventContent (XML.ContentText (get x))
  , XML.EventEndElement content
  , XML.EventEndElement document
  ]
  where
    document :: XML.Name
    document = toName "document"
    content :: XML.Name
    content = toName "content"


startEvents :: [XML.Event]
startEvents =
  [ XML.EventBeginDocument
  , XML.EventBeginElement docset []
  , XML.EventBeginElement schema []
  , XML.EventBeginElement field [("name", [XML.ContentText "content"])]
  , XML.EventEndElement field
  , XML.EventEndElement schema
  ]
  where
    schema :: XML.Name
    schema = toName "schema"
    field :: XML.Name
    field = toName "field"


endEvents :: [XML.Event]
endEvents =
  [ XML.EventEndElement docset
  ]


toName :: Text -> XML.Name
toName x = XML.Name x (Just "http://sphinxsearch.com/") (Just "sphinx")

docset :: XML.Name
docset = toName "docset"
