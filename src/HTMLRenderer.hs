{-|
Module         : HTMLRenderer
Description    : Rendering HTML-report from listings.
Copyright      : (c) Aleksi Tarvainen, 2020
License        : BSD3
Maintainer     : aleksi@atarv.dev
-}
{-# LANGUAGE OverloadedStrings #-}
module HTMLRenderer (sectionToHtml, sectionsToHtml, errorSectionToHtml) where

import           Control.Monad                   (forM_)
import qualified Data.Text                       as T
import           Listing
import           StyleGen
import           Text.Blaze.Html.Renderer.Pretty
import           Text.Blaze.Html5                as H
import           Text.Blaze.Html5.Attributes     as A

listingsSection :: T.Text -> Html
listingsSection sectionTitle = H.div ! A.class_ "section" $ do
    H.h2 $ text sectionTitle

sectionToHtml :: T.Text -> [Listing] -> String
sectionToHtml sectionTitle [] = renderHtml $ do
    listingsSection sectionTitle
    H.p $ H.em "Ei uusia ilmoituksia"
sectionToHtml sectionTitle listings = renderHtml $ do
    listingsSection sectionTitle
    H.div ! A.class_ "announcement-list" $ forM_ listings toHtml

errorSectionToHtml :: T.Text -> T.Text -> String
errorSectionToHtml sectionTitle errorMessage = renderHtml $ do
    listingsSection sectionTitle
    H.p $ H.em ! A.class_ "section-error" $ "Virhe: " <> text errorMessage

sectionsToHtml :: String -> String -> String
sectionsToHtml topTitle sectionsHtml = renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title "M.net-päivystäjä - uusia ilmoituksia"
        H.style $ lazyText styleSheet
    body $ do
        h1 $ string topTitle
        preEscapedString sectionsHtml
