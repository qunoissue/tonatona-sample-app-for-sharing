module TonaApp.MongoDB
  ( Config
  , Dsl
  , run
  ) where

import Tonalude
import Tonatona (HasConfig(..), HasParser(..))
import TonaParser ((.||), argLong, envVar, requiredVal)

import qualified Database.MongoDB as MongoDB



type Dsl env
  = MongoDB.Action (RIO env)


-- | Main function.
run
  :: (HasConfig env Config)
  => (MongoDB.Failure -> RIO env a) -- ^ Error handler
  -> MongoDB.AccessMode
  -> MongoDB.Database
  -> Dsl env a
  -> RIO env a
run errorHandler accessMode database query = do
  Config {..} <- asks config
  handle errorHandler $ do
    host <- liftIO $ MongoDB.readHostPortM $ unConnString connString
    pipe <- liftIO $ MongoDB.connect host
    res <- MongoDB.access
      pipe
      accessMode
      database $ do
        _ <- MongoDB.auth (unUserName userName) (unPassword password)
        query
    liftIO $ MongoDB.close pipe
    pure res



-- Config


data Config = Config
  { connString :: ConnString
  , userName :: Username
  , password :: Password
  }


newtype ConnString = ConnString
  { unConnString :: String
  } deriving (Eq, Show)

instance HasParser ConnString where
  parser = ConnString <$>
    requiredVal
      "Formatted string to connect MongoDB (e.g. \"localhost:27017\")"
      (argLong "mongodb-conn-string" .|| envVar "MONGODB_CONN_STRING")


newtype Username = Username
  { unUserName :: MongoDB.Username
  } deriving (Eq, Show)

instance HasParser Username where
  parser = Username <$>
    requiredVal
      "Username to connect MongoDB"
      (argLong "mongodb-username" .|| envVar "MONGODB_USERNAME")


newtype Password = Password
  { unPassword :: MongoDB.Password
  } deriving (Eq, Show)

instance HasParser Password where
  parser = Password <$>
    requiredVal
      "User password to connect MongoDB"
      (argLong "mongodb-password" .|| envVar "MONGODB_PASSWORD")


instance HasParser Config where
  parser = Config
      <$> parser
      <*> parser
      <*> parser
