{-# LANGUAGE OverloadedStrings #-}
module HTMLRenderer (announcementsToHtml) where

import           Announcement
import           Control.Monad                  ( forM_ )
import           Data.Time
import           StyleGen
import           Text.Blaze.Html.Renderer.Pretty
import           Text.Blaze.Html5              as H
import           Text.Blaze.Html5.Attributes   as A
import qualified Data.Text                     as T

announcementsToHtml :: String -> [Announcement] -> String
announcementsToHtml title anns = renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title "M.net-päivystäjä - uusia ilmoituksia"
        H.style $ lazyText styleSheet
    body $ do
        h1 $ string title
        H.div ! A.class_ "announcement-list" $ forM_ anns toHtml
