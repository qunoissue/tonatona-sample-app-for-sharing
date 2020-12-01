{-# LANGUAGE TemplateHaskell #-}

{-|
Description : Functions for sending email using SES.

Functions for sending email using SES.
-}

module TonaApp.Mail
    ( EmailAddress(..)
    , mkAddress
    , sendMailWithSendmail
    , simpleMailWithReplyTo
    ) where

import Tonalude

import qualified Tonatona.Logger as TonaLogger
-- import Control.Concurrent (forkIO)
import Control.Concurrent (threadDelay)
import UnliftIO.Concurrent (forkIO)
import Network.Mail.Mime

import TonaApp.Config (AppM)



-- | Simple newtype wrapper for an email address.  We don't need validation so
-- we don't use 'Text.Email.Validate'.
newtype EmailAddress = EmailAddress { unEmailAddress :: Text }
    deriving Show


sendMailWithSendmail :: EmailAddress -> EmailAddress -> Mail -> AppM ()
sendMailWithSendmail _toEmailAddr _fromEmailAddr mail =
    void . forkIO $ do
      -- TonaLogger.logDebug $ display ("sending" :: Text)
      liftIO $ renderSendMail mail
      TonaLogger.logDebug $ display ("sent" :: Text)

-- | Simple wrapper for creating a 'Mail' with a @Reply-To@ header and a plain-text body.
simpleMailWithReplyTo :: Address -> Address -> Address -> Text -> LText -> Mail
simpleMailWithReplyTo to from replyTo subject body =
    let mail = simpleMail' to from subject body
    in mail { mailHeaders = mailHeaders mail <> [("Reply-To", renderAddress replyTo)] }


-- | Helper to make an 'Address' with no name.
mkAddress :: Text -> Address
mkAddress email = Address { addressName = Nothing, addressEmail = email }





_dummyOfRenderSendMail :: Mail -> IO ()
_dummyOfRenderSendMail _ =
  sleep 3

sleep :: Int -> IO ()
sleep = threadDelay . (1000 * 1000 *)
