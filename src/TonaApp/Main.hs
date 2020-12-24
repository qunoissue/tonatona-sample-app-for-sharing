{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TonaApp.Main where

import Tonalude

import Tonatona (HasConfig(..))
import qualified Tonatona.Logger as TonaLogger
import qualified Tonatona.Persist.Postgresql as TonaDb
import qualified Tonatona.Servant as TonaServant

-- import Database.Persist.Sql
import Servant

import TonaApp.Config (AppM)
import TonaApp.Db.EntityDefs
import TonaApp.Type



-- App


app :: AppM ()
app = do
  port <- asks (TonaServant.port . config)
  TonaLogger.logDebug $
    ("About to run web server on port " <> display port <> " ...")
  TonaLogger.logDebug $ display ("About to run migration..." :: Text)
  TonaDb.runMigrate migrateAll
  TonaServant.run @API server

type API = Get '[JSON] Response

server :: ServerT API AppM
server = main



main :: AppM Response
main = do
  -- cid <- TonaDb.run $
  --   insert $ Company "uzuz" "0300000000" "data1"
  -- TonaDb.run $
  --   insert_ $ Employee "tomone" "08000000000" cid
  pure ()
