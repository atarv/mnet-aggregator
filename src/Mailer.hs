{-|
Module         : Mailer
Description    : Sending mail via AWS SES.
Copyright      : (c) Aleksi Tarvainen, 2024
License        : BSD3
Maintainer     : aleksi@atarv.dev
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Mailer (sendListingMail) where
import qualified Amazonka
import           Amazonka.Prelude
import           Amazonka.SESV2
import           Amazonka.SESV2.Lens
import           Configs
import           Control.Lens
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as LT
import qualified Data.Text.Lazy.IO    as LT
import           Network.Mail.Mime    as M
import           ScrapingOptions


-- | Send mail containing HTML
sendListingMail :: Amazonka.Env 
    -> MailConfig 
    -> ScrapingOptions 
    -> LT.Text 
    -> IO ()
sendListingMail awsEnv MailConfig{..} ScrapingOptions{..} html = do
    mail <- LB.toStrict <$> (M.renderMail' =<< M.simpleMail 
                (M.Address (Just recipientName) recipientEmail)
                (M.Address (Just senderName) senderEmail)
                "M.net-päivystäjän raportti"
                "Saatavilla vain HTML-muodossa"
                html
                []) -- no attachments

    let content = newEmailContent & emailContent_raw ?~ newRawMessage mail
        sendEmailRequest = newSendEmail content

    response <- Amazonka.runResourceT $ Amazonka.send awsEnv sendEmailRequest
    putStrLn $ "Sent email report to '"
        <> T.unpack recipientEmail
        <> "', message id ("
        <> show (response ^. sendEmailResponse_messageId)
        <> "), status "
        <> show (response ^. sendEmailResponse_httpStatus)
