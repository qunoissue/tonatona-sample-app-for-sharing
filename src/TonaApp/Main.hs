{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TonaApp.Main where

import Tonalude

import Tonatona (HasConfig(..))
import qualified Tonatona.Logger as TonaLogger
import qualified Tonatona.Servant as TonaServant

import Control.Concurrent (threadDelay)
import Servant
import UnliftIO.Concurrent (forkIO)

import TonaApp.Config (AppM)
import TonaApp.Mail



-- App


app :: AppM ()
app = do
  port <- asks (TonaServant.port . config)
  TonaLogger.logDebug $
    ("About to run web server on port " <> display port <> " ...")
  TonaServant.run @API server

type API = Get '[JSON] ()

server :: ServerT API AppM
server = main



main :: AppM ()
main = do
  someProcess
  TonaLogger.logDebug $ display ("barrrrr" :: Text)
  pure ()

someProcess :: AppM ()
someProcess = do
  void . forkIO $ process5sec
  mailProcess
  TonaLogger.logDebug $ display ("foooo" :: Text)
  void . forkIO $ process5sec
  TonaLogger.logDebug $ display ("bar" :: Text)
  pure ()

mailProcess :: AppM ()
mailProcess = do
  let
    to = mkAddress "akashiz2224@gmail.com"
    from = mkAddress "akashiz2224@gmail.com"
    replyTo = mkAddress "akashiz2224@gmail.com"
    to' = EmailAddress "akashiz2224@gmail.com"
    from' = EmailAddress "akashiz2224@gmail.com"
    mail = simpleMailWithReplyTo to from replyTo "test subject" "foo bar baz"
  sendMailWithSendmail to' from' mail


process5sec :: AppM ()
process5sec = do
  TonaLogger.logDebug $ display ("process5sec started." :: Text)
  sleep 5
  TonaLogger.logDebug $ display ("process5sec finished." :: Text)
  pure ()


sleep :: Int -> RIO env ()
sleep = liftIO . threadDelay . (1000 * 1000 *)
