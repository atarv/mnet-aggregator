module Announcement (Announcement(..)) where
import qualified Data.Text                     as T

data Announcement =
    Announcement { title :: T.Text, announcementId :: T.Text
                 , description :: T.Text, author :: T.Text, authorId :: T.Text
                 , price :: T.Text, thumbnails :: [T.Text], dates :: T.Text
                 }
    deriving (Show)

instance Eq Announcement where
    (==) a b = announcementId a == announcementId b
