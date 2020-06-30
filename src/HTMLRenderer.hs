{-# LANGUAGE OverloadedStrings #-}
module HTMLRenderer (announcementsToHtml) where

import           Announcement
import           Control.Monad                  ( forM_ )
import           Text.Blaze.Html5              as H
import           Text.Blaze.Html5.Attributes   as A
import           Text.Blaze.Html.Renderer.Pretty

announcementsToHtml :: [Announcement] -> String
announcementsToHtml anns = renderHtml $ docTypeHtml $ do
    H.head $ H.title "M.net-päivystäjä - uusia ilmoituksia"
    body $ do
        h1 $ text "Uusia ilmoituksia"
        H.div ! A.class_ "announcement-list" $ forM_ anns toHtml
