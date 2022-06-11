module TonaApp.Main where

import Tonalude

import qualified Database.MongoDB as MongoDB
import TonaApp.Config (AppM)
import qualified TonaApp.NewMeetingCounselor as NewMeetingCounselor
import qualified TonaApp.MongoDB.Extra as MongoDB
import qualified Tonatona.Logger as TonaLogger



-- App


app :: AppM ()
app = do
  result <-
    NewMeetingCounselor.find
      errorHandler $
      MongoDB.Query'
        { conditions = ["team.isLeader" MongoDB.=: True]
        , project = []
        , limit = 0
        , sort = []
        }
  TonaLogger.logDebug $ display ("新規面談カウンセラー一覧: " :: Text)
  TonaLogger.logDebug $ display $ tshow result
  pure ()


errorHandler
  :: MongoDB.Failure
  -> AppM a
errorHandler e = do
  TonaLogger.logError $ display $ tshow e
  throwM e
