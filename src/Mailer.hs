{-|
Module         : Mailer
Description    : Sending mail through SMTP.
Copyright      : (c) Aleksi Tarvainen, 2020
License        : BSD3
Maintainer     : aleksi@atarv.dev
-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Mailer (sendListingMail) where
import           Configs
import           Network.Mail.Mime             as M
import           Network.Mail.SMTP
import           ScrapingOptions
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT


-- | Send mail containing HTML
sendListingMail :: MailConfig -> ScrapingOptions -> LT.Text -> IO ()
sendListingMail MailConfig {..} ScrapingOptions {..} html =
    M.simpleMail (M.Address (Just recipientName) recipientEmail)
                 (M.Address (Just senderName) senderEmail)
                 "M.net-päivystäjän raportti"
                 "Saatavilla vain HTML-muodossa"
                 html
                 [] -- no attachments
        >>= sendMailWithLogin' (T.unpack smtpHostname)
                               (fromIntegral smtpPort)
                               (T.unpack smtpUsername)
                               (T.unpack smtpPassword)
