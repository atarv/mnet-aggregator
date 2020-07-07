{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Mailer (sendAnnouncementMail) where
import           Configs
import           Network.Mail.SMTP
import           Network.Mail.Mime             as M
import qualified Data.Text.Lazy                as LT
import qualified Data.Text                     as T
import           ScrapingOptions


sendAnnouncementMail :: MailConfig -> ScrapingOptions -> LT.Text -> IO ()
sendAnnouncementMail MailConfig {..} ScrapingOptions {..} html =
    M.simpleMail (M.Address (Just recipientName) recipientEmail)
                 (M.Address (Just senderName) senderEmail)
                 "M.net-päivystäjän raportti"
                 "Saatavilla vain HTML-muodossa"
                 html
                 []
        >>= sendMailWithLogin' (T.unpack smtpHostname)
                               587
                               (T.unpack smtpUsername)
                               (T.unpack smtpPassword)
