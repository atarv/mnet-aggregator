{-|
Module         : StyleGen
Description    : Generates inline CSS styles for Gmail-compatible HTML emails
Copyright      : (c) Aleksi Tarvainen, 2020
License        : BSD3
Maintainer     : aleksi@atarv.dev
-}
{-# LANGUAGE OverloadedStrings #-}
module StyleGen
    ( inlineStyleAttr
    , bodyStyle
    , h1Style
    , h2Style
    , h3Style
    , h3LinkStyle
    , sectionStyle
    , sectionErrorStyle
    , announcementStyle
    , announcementBodyPStyle
    , announcementAuthorStyle
    , announcementAuthorLinkStyle
    , announcementMetaStyle
    , announcementImageStyle
    , noListingsStyle
    ) where

import           Clay
import           Clay.Render                 (htmlInline)
import           Data.String                 (IsString)
import qualified Data.Text.Lazy              as LT
import           Prelude                     hiding (div, span, (**))
import           Text.Blaze.Html5            (Attribute, lazyTextValue)
import qualified Text.Blaze.Html5.Attributes as A

-- | Render a Clay Css block to a Blaze inline style attribute
inlineStyleAttr :: Css -> Attribute
inlineStyleAttr = A.style . lazyTextValue . renderWith htmlInline []

fontStack :: IsString s => [s]
fontStack = ["Open Sans", "Verdana", "Geneva", "Arial", "Helvetica"]

-- Colors
colorPrimary :: Color
colorPrimary = parse "#2c7a7b"

colorPrimaryDark :: Color
colorPrimaryDark = parse "#1a4a4b"

colorError :: Color
colorError = parse "#c0392b"

colorText :: Color
colorText = grayish 40

colorMuted :: Color
colorMuted = grayish 100

colorBorder :: Color
colorBorder = grayish 220

colorBackground :: Color
colorBackground = grayish 245

colorSurface :: Color
colorSurface = white

-- Individual style blocks for inline use

bodyStyle :: Css
bodyStyle = do
    maxWidth (px 680)
    margin (px 0) auto (px 0) auto
    fontFamily fontStack [sansSerif]
    fontSize (px 14)
    color colorText
    backgroundColor colorBackground

h1Style :: Css
h1Style = do
    fontFamily fontStack [sansSerif]
    background colorPrimary
    color white
    fontSize (px 26)
    padding (px 10) (px 16) (px 10) (px 16)
    marginBottom (px 0)

h2Style :: Css
h2Style = do
    fontFamily fontStack [sansSerif]
    color colorPrimary
    borderBottom (px 1) dotted colorPrimary
    paddingBottom (px 4)
    fontSize (px 18)

h3Style :: Css
h3Style = do
    fontFamily fontStack [sansSerif]
    color colorPrimaryDark
    margin (px 0) (px 0) (px 8) (px 0)
    fontSize (px 16)
    borderBottom (px 1) dotted colorPrimaryDark
    paddingBottom (px 4)

h3LinkStyle :: Css
h3LinkStyle = do
    color colorPrimary
    textDecoration none

sectionStyle :: Css
sectionStyle =
    padding (px 0) (px 8) (px 0) (px 8)

sectionErrorStyle :: Css
sectionErrorStyle = do
    color colorError
    fontStyle italic

announcementStyle :: Css
announcementStyle = do
    backgroundColor colorSurface
    border (px 1) solid colorBorder
    padding (px 14) (px 16) (px 14) (px 16)
    marginBottom (px 12)
    overflow hidden

announcementBodyPStyle :: Css
announcementBodyPStyle = do
    margin (px 8) (px 0) (px 8) (px 0)
    lineHeight (unitless 1.5)

announcementAuthorStyle :: Css
announcementAuthorStyle = do
    fontSize (px 12)
    color colorMuted

announcementAuthorLinkStyle :: Css
announcementAuthorLinkStyle =
    color colorPrimary

announcementMetaStyle :: Css
announcementMetaStyle = do
    fontSize (px 12)
    color colorMuted
    margin (px 4) (px 0) (px 4) (px 0)

announcementImageStyle :: Css
announcementImageStyle = do
    maxWidth (pct 38)
    marginLeft (px 12)
    marginBottom (px 8)
    border (px 1) solid colorBorder

noListingsStyle :: Css
noListingsStyle =
    fontStyle italic
