{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Announcement (Announcement(..)) where
import           Prelude                 hiding ( div
                                                , span
                                                )
import           Configs
import qualified Data.Text                     as T
import           Text.Blaze.Html
import           Text.Blaze.Html5.Attributes   as A
import           Text.Blaze.Html5              as H

data Announcement =
    Announcement { title :: T.Text, announcementId :: T.Text
                 , description :: T.Text, author :: T.Text, authorId :: T.Text
                 , price :: T.Text, thumbnails :: [T.Text], dates :: T.Text
                 }
    deriving (Show)

instance Eq Announcement where
    (==) a b = announcementId a == announcementId b

instance ToMarkup Announcement where
    toMarkup Announcement {..} = div ! class_ "announcement" $ do
        h3 $ a ! href (toValue $ T.pack baseUrl <> announcementId) $ text title
        H.span $ do
            text "Ilmoittaja: "
            a ! href (toValue $ T.pack baseUrl <> authorId) $ text author
        p $ text description
        H.span $ text $ "Hinta: " <> price
        H.span $ text dates
        div ! class_ "thumbnails" $ mapM_
            (\t -> img ! src (toValue $ T.pack baseUrl <> t) ! alt
                "announcement image"
            )
            thumbnails
        hr
