{-# LANGUAGE OverloadedStrings #-}
module Mailer (sendAnnouncementMail) where
import           Network.Mail.SMTP
import           Network.Mail.Mime             as M
import qualified Data.Text.Lazy                as LT

type PlainTextPart = LT.Text
type HTMLPart = LT.Text

constructMail :: PlainTextPart -> HTMLPart -> IO Mail
constructMail plaintext html = M.simpleMail (M.Address (Just "Vastaanottaja") "@")
                                   (M.Address (Just "Lähettäjä") "@")
                                   "Uusia ilmoituksia"
                                   plaintext
                                   html
                                   []

sendAnnouncementMail :: HTMLPart -> IO ()
sendAnnouncementMail html = do
    message <- constructMail "Saatavilla vain HTML-muodossa" html
    sendMailWithLogin'
        "hostname"
        587
        "username"
        "password"
        message
