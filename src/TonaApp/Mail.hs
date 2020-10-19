{-# LANGUAGE TemplateHaskell #-}

{-|
Description : Functions for sending email using SES.

Functions for sending email using SES.
-}

module TonaApp.Mail
    ( EmailAddress(..)
    , mkAddress
    , sendMailWithSendmail
    , simpleHtmlMailWithReplyTo
    , simpleMailWithReplyTo
    ) where

import Tonalude

-- import Control.Concurrent (forkIO)
import UnliftIO.Concurrent (forkIO)
import Network.Mail.Mime

import TonaApp.Config (AppM)



-- | Simple newtype wrapper for an email address.  We don't need validation so
-- we don't use 'Text.Email.Validate'.
newtype EmailAddress = EmailAddress { unEmailAddress :: Text }
    deriving Show


sendMailWithSendmail :: EmailAddress -> EmailAddress -> Mail -> AppM ()
sendMailWithSendmail _toEmailAddr _fromEmailAddr mail =
    void . forkIO . liftIO $ renderSendMail mail

-- | Simple wrapper for creating a 'Mail' with a @Reply-To@ header and a plain-text body.
simpleMailWithReplyTo :: Address -> Address -> Address -> Text -> LText -> Mail
simpleMailWithReplyTo to from replyTo subject body =
    let mail = simpleMail' to from subject body
    in mail { mailHeaders = mailHeaders mail <> [("Reply-To", renderAddress replyTo)] }


-- | Simple wrapper for creating a 'Mail' with a @Reply-To@ header and an HTML body.
simpleHtmlMailWithReplyTo :: Address -> Address -> Address -> Text -> LText -> Mail
simpleHtmlMailWithReplyTo to from replyTo subject body =
    let
      mail = simpleMailInMemory to from subject "" body []
    in
    mail { mailHeaders = mailHeaders mail <> [("Reply-To", renderAddress replyTo)] }


-- | Helper to make an 'Address' with no name.
mkAddress :: Text -> Address
mkAddress email = Address { addressName = Nothing, addressEmail = email }
