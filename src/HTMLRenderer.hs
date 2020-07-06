{-# LANGUAGE OverloadedStrings #-}
module HTMLRenderer (sectionToHtml, sectionsToHtml) where

import           Announcement
import           Control.Monad                  ( forM_ )
import           StyleGen
import           Text.Blaze.Html.Renderer.Pretty
import           Text.Blaze.Html5              as H
import           Text.Blaze.Html5.Attributes   as A
import qualified Data.Text                     as T

sectionToHtml :: T.Text -> [Announcement] -> String
sectionToHtml sectionTitle [] = renderHtml $ H.div ! A.class_ "section" $ do
    H.h2 $ text sectionTitle
    H.p $ H.em "Ei uusia ilmoituksia"
sectionToHtml sectionTitle announcements =
    renderHtml $ H.div ! A.class_ "section" $ do
        H.h2 $ text sectionTitle
        H.div ! A.class_ "announcement-list" $ forM_ announcements toHtml

sectionsToHtml :: String -> String -> String
sectionsToHtml topTitle sectionsHtml = renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title "M.net-päivystäjä - uusia ilmoituksia"
        H.style $ lazyText styleSheet
    body $ do
        h1 $ string topTitle
        preEscapedString sectionsHtml
