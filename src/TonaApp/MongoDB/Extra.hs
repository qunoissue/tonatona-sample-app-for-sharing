module TonaApp.MongoDB.Extra
    ( Query'(..)
    , find'
    ) where

import Tonalude

import qualified Database.MongoDB as MongoDB
import qualified TonaApp.MongoDB as TonaMongoDB



data Query' = Query'
  { conditions :: MongoDB.Selector
  , project :: MongoDB.Projector
  , limit :: MongoDB.Limit
  , sort :: MongoDB.Order
  }


find'
  :: MongoDB.Collection
  -> Query'
  -> TonaMongoDB.Dsl env [MongoDB.Document]
find' collection Query' {..} =
  MongoDB.rest =<< MongoDB.find
    ( MongoDB.select
        conditions
        collection
    )
    { MongoDB.project = project
    , MongoDB.limit = limit
    , MongoDB.sort = sort
    }
