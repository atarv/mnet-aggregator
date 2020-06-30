{-# LANGUAGE OverloadedStrings, RecordWildCards, StrictData #-}
module Announcement (Announcement(..)) where
import           Prelude                 hiding ( div
                                                , span
                                                )
import           Configs
import           Text.Blaze.Html
import           Text.Blaze.Html5              as H
import           Text.Blaze.Html5.Attributes   as A
import qualified Data.Text                     as T

data Announcement =
    Announcement { announcementId :: !T.Text
                 , author :: !T.Text
                 , authorId :: !T.Text
                 , dates :: !T.Text
                 , description :: !T.Text
                 , thumbnails :: ![T.Text]
                 , title :: !T.Text
                 }
    deriving (Show)

instance Eq Announcement where
    (==) a b = announcementId a == announcementId b

instance ToMarkup Announcement where
    toMarkup Announcement {..} = div ! class_ "announcement" $ do
        h2 $ a ! href (toValue $ T.pack baseUrl <> announcementId) $ text title
        div $ do
            H.span $ do
                text "Ilmoittaja: "
                a ! href (toValue $ T.pack baseUrl <> authorId) $ text author
            mapM_
                (\t -> img ! src (toValue $ T.pack baseUrl <> t) ! alt
                    "announcement image"
                )
                thumbnails
            p $ preEscapedText description
            H.span $ text dates
        hr
