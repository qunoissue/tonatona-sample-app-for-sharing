
module TonaApp.Config
  (AppM
  ) where

import Tonalude
import Tonatona (HasConfig(..), HasParser(..))

import qualified Tonatona.Logger as TonaLogger
import qualified Tonatona.Servant as TonaServant



type AppM = RIO Config



-- Config


data Config = Config
  { tonaLogger :: TonaLogger.Config
  , tonaServant :: TonaServant.Config
  -- , anotherPlugin :: TonaAnotherPlugin.Config
  }

instance HasConfig Config TonaLogger.Config where
  config = tonaLogger

instance HasConfig Config TonaServant.Config where
  config = tonaServant

instance HasParser Config where
  parser = Config
      <$> parser
      <*> parser
      -- <*> parser
