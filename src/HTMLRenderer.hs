{-|
Module         : HTMLRenderer
Description    : Rendering HTML-report from listings (Gmail-compatible inline styles).
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
listingsSection sectionTitle = H.div ! inlineStyleAttr sectionStyle $
    H.h2 ! inlineStyleAttr h2Style $ text sectionTitle

sectionToHtml :: T.Text -> [Listing] -> String
sectionToHtml sectionTitle [] = renderHtml $ do
    listingsSection sectionTitle
    H.p $ H.em ! inlineStyleAttr noListingsStyle $ "Ei uusia ilmoituksia"
sectionToHtml sectionTitle listings = renderHtml $ do
    listingsSection sectionTitle
    forM_ listings toHtml

errorSectionToHtml :: T.Text -> T.Text -> String
errorSectionToHtml sectionTitle errorMessage = renderHtml $ do
    listingsSection sectionTitle
    H.p $ H.em ! inlineStyleAttr sectionErrorStyle $ "Virhe: " <> text errorMessage

sectionsToHtml :: String -> String -> String
sectionsToHtml topTitle sectionsHtml = renderHtml $ docTypeHtml $ do
    H.head $ do
        H.meta ! httpEquiv "Content-Type" ! content "text/html; charset=utf-8"
        H.meta ! name "viewport" ! content "width=device-width, initial-scale=1"
        H.title "M.net-päivystäjä - uusia ilmoituksia"
    body ! inlineStyleAttr bodyStyle $ do
        h1 ! inlineStyleAttr h1Style $ string topTitle
        preEscapedString sectionsHtml
