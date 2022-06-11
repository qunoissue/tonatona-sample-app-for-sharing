module TonaApp.NewMeetingCounselor
  ( NewMeetingCounselor (..)
  , Team (..)
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
tag = "new-meeting-counselor"


-- |
data NewMeetingCounselor = NewMeetingCounselor
  { objectId :: Bson.ObjectId
  , email :: Text
  , name :: Text
  , mailName :: Text
  , chatwork :: Text
  , line :: Text
  , maxNewMeetingCount :: Int
  , canHandleHumanitiesGraduates :: Bool
  , canHandleScienceGraduates :: Bool
  , newMeetingMemo :: Text
  , newMeetingAssignPriority :: Int
  , team :: Maybe Team
  , portersEmployee :: Text
  , portersCounselor :: Text
  } deriving (Eq, Show)

parse :: Bson.Document -> Bson.ParseResult NewMeetingCounselor
parse doc =
  let
    parsePortersAlias :: Bson.Document -> Bson.ParseResult Text
    parsePortersAlias =
      Bson.lookup "alias"
  in
  NewMeetingCounselor
    <$> Bson.lookup "_id" doc
    <*> Bson.lookup "email" doc
    <*> Bson.lookup "name" doc
    <*> Bson.lookup "mailName" doc
    <*> Bson.lookup "chatwork" doc
    <*> Bson.lookup "line" doc
    <*> Bson.lookup "maxNewMeetingCount" doc
    <*> Bson.lookup "canHandleHumanitiesGraduates" doc
    <*> Bson.lookup "canHandleScienceGraduates" doc
    <*> Bson.lookup "newMeetingMemo" doc
    <*> Bson.lookup "newMeetingAssignPriority" doc
    <*> (traverse parseTeam =<< Bson.lookupOptional "team" doc)
    <*> (parsePortersAlias =<< Bson.lookup "portersEmployee" doc)
    <*> (parsePortersAlias =<< Bson.lookup "portersCounselor" doc)


-- |
data Team = Team
  { name :: Text
  , isLeader :: Bool
  } deriving (Eq, Show)

parseTeam :: Bson.Document -> Bson.ParseResult Team
parseTeam doc =
  Team
    <$> Bson.lookup "name" doc
    <*> Bson.lookup "isLeader" doc


-- |
find
  :: HasConfig env TonaMongoDB.Config
  => (MongoDB.Failure -> RIO env [Bson.ParseResult NewMeetingCounselor])
  -> MongoDB.Query'
  -> RIO env [Bson.ParseResult NewMeetingCounselor]
find errorHandler query =
  TonaMongoDB.run errorHandler MongoDB.master "uzuz" $ do
    docs <- MongoDB.find' "employee" $
      query
        { MongoDB.conditions =
            ("tags" MongoDB.=: tag) : MongoDB.conditions query
        }
    pure $ map parse docs
