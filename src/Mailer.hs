{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Mailer (sendAnnouncementMail) where
import           Configs
import           Network.Mail.SMTP
import           Network.Mail.Mime             as M
import qualified Data.Text.Lazy                as LT
import qualified Data.Text                     as T


sendAnnouncementMail :: Config -> LT.Text -> IO ()
sendAnnouncementMail Config {..} html =
    M.simpleMail (M.Address (Just recipientName) recipientEmail)
                 (M.Address (Just senderName) senderEmail)
                 "Uusia ilmoituksia"
                 "Saatavilla vain HTML-muodossa"
                 html
                 []
        >>= sendMailWithLogin' (T.unpack hostname)
                               587
                               (T.unpack smtpUsername)
                               (T.unpack smtpPassword)