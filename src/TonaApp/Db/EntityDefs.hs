{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module TonaApp.Db.EntityDefs where

import Tonalude

import Database.Persist.TH (mkMigrate, mkPersist, mpsGenerateLenses, persistLowerCase, share, sqlSettings)


$(share
  [ mkPersist sqlSettings {mpsGenerateLenses = False}
  , mkMigrate "migrateAll"
  ]
  [persistLowerCase|
  Company
    name             Text
    tel              Text
    -- data1            Text

    deriving Eq
    deriving Show

  -- Employee
  --   name             Text
  --   tel              Text
  --   company          CompanyId
  --
  --   deriving Eq
  --   deriving Show
  |]
 )
