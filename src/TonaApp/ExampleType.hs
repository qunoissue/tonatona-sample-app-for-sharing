module TonaApp.ExampleType
  ( ExampleType
  , find
  ) where

import Tonalude

import qualified Data.Bson as Bson
import qualified Database.MongoDB as MongoDB
import qualified TonaApp.Bson.Extra as Bson
import qualified TonaApp.MongoDB.Extra as MongoDB
import qualified TonaApp.MongoDB as TonaMongoDB
import Tonatona (HasConfig)



tag :: Text
tag = "example-tag"


data ExampleType = ExampleType
  { objectId :: Bson.ObjectId
  , someRequired :: SomeRequired
  , someOptional :: Maybe SomeOptional
  , someList :: [SomeList]
  } deriving (Eq, Show)

parse :: Bson.Document -> Bson.ParseResult ExampleType
parse doc =
  ExampleType
    <$> Bson.lookup "_id" doc
    <*> (parseSomeRequired =<< Bson.lookup "someRequired" doc)
    <*> (traverse parseSomeOptional =<< Bson.lookupOptional "someOptional" doc)
    <*> (traverse parseSomeList =<< Bson.lookup "someList" doc)


data SomeRequired = SomeRequired
  { foo :: Text
  , bar :: Int
  } deriving (Eq, Show)

parseSomeRequired :: Bson.Document -> Bson.ParseResult SomeRequired
parseSomeRequired doc =
  SomeRequired
    <$> Bson.lookup "foo" doc
    <*> Bson.lookup "bar" doc


newtype SomeOptional = SomeOptional
  { baz :: Text
  } deriving (Eq, Show)

parseSomeOptional :: Bson.Document -> Bson.ParseResult SomeOptional
parseSomeOptional doc =
  SomeOptional
    <$> Bson.lookup "baz" doc


newtype SomeList = SomeList
  { qux :: Text
  } deriving (Eq, Show)

parseSomeList :: Bson.Document -> Bson.ParseResult SomeList
parseSomeList doc =
  SomeList
    <$> Bson.lookup "qux" doc


find
  :: HasConfig env TonaMongoDB.Config
  => (MongoDB.Failure -> RIO env [Bson.ParseResult ExampleType])
  -> MongoDB.Query'
  -> RIO env [Bson.ParseResult ExampleType]
find errorHandler query =
  TonaMongoDB.run errorHandler MongoDB.master "uzuz" $ do
    docs <- MongoDB.find' "example" $
      query
        { MongoDB.conditions =
            ("tags" MongoDB.=: tag) : MongoDB.conditions query
        }
    pure $ map parse docs
