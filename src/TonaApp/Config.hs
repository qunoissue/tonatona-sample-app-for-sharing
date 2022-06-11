
module TonaApp.Config
  ( AppM
  ) where

import Tonalude
import Tonatona (HasConfig(..), HasParser(..))

import qualified Tonatona.Logger as TonaLogger
import qualified TonaApp.MongoDB as TonaDb



type AppM = RIO Config



-- Config


data Config = Config
  { tonaLogger :: TonaLogger.Config
  , tonaDb :: TonaDb.Config
  -- , anotherPlugin :: TonaAnotherPlugin.Config
  }

instance HasConfig Config TonaLogger.Config where
  config = tonaLogger

instance HasConfig Config TonaDb.Config where
  config = tonaDb

instance HasParser Config where
  parser = Config
      <$> parser
      <*> parser
      -- <*> parser
