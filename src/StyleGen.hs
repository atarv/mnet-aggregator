{-|
Module         : StyleGen
Description    : Generates the CSS-stylesheet for the report
Copyright      : (c) Aleksi Tarvainen, 2020
License        : BSD3
Maintainer     : aleksi@atarv.dev
-}
{-# LANGUAGE OverloadedStrings #-}
module StyleGen (styleSheet) where

import           Clay
import           Data.String    (IsString)
import qualified Data.Text.Lazy as LT
import           Prelude        hiding (div, span, (**))

styleSheet :: LT.Text
styleSheet = render mnetStyle

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

mnetStyle :: Css
mnetStyle = do
    body ? do
        maxWidth (px 680)
        margin (px 0) auto (px 0) auto
        fontFamily fontStack [sansSerif]
        fontSize (px 14)
        color colorText
        backgroundColor colorBackground
    h1 ? do
        fontFamily fontStack [sansSerif]
        background colorPrimary
        color white
        fontSize (px 26)
        padding (px 10) (px 16) (px 10) (px 16)
        marginBottom (px 0)
    h2 ? do
        fontFamily fontStack [sansSerif]
        color colorPrimary
        borderBottom (px 1) dotted colorPrimary
        paddingBottom (px 4)
        fontSize (px 18)
    h3 ? do
        fontFamily fontStack [sansSerif]
        color colorPrimaryDark
        margin (px 0) (px 0) (px 8) (px 0)
        fontSize (px 16)
        borderBottom (px 1) dotted colorPrimaryDark
        paddingBottom (px 4)
    h3 ** a ? do
        color colorPrimary
        textDecoration none
        cursor pointer
    div # byClass "section" ? do
        padding (px 0) (px 8) (px 0) (px 8)
    "em" # byClass "section-error" ? do
        color colorError
        fontStyle italic
    div # byClass "announcement" ? do
        backgroundColor colorSurface
        border (px 1) solid colorBorder
        padding (px 14) (px 16) (px 14) (px 16)
        marginBottom (px 12)
    table # byClass "announcement-table" ? do
        width (pct 100)
        borderCollapse collapse
    td # byClass "announcement-body" ? do
        verticalAlign vAlignTop
    (td # byClass "announcement-body") ** p ? do
        margin (px 8) (px 0) (px 8) (px 0)
        lineHeight (unitless 1.5)
    span # byClass "announcement-author" ? do
        fontSize (px 12)
        color colorMuted
    (span # byClass "announcement-author") ** a ? do
        color colorPrimary
    (p # byClass "announcement-meta") ? do
        fontSize (px 12)
        color colorMuted
        margin (px 4) (px 0) (px 4) (px 0)
    td # byClass "announcement-image" ? do
        width (pct 38)
        verticalAlign vAlignTop
        paddingLeft (px 12)
    (td # byClass "announcement-image") ** img ? do
        maxWidth (pct 100)
        border (px 1) solid colorBorder
    img ? do
        maxWidth (pct 100)
