{-# LANGUAGE OverloadedStrings #-}
module StyleGen (styleSheet) where
import           Prelude                 hiding ( div
                                                , span
                                                )
import           Clay
import qualified Data.Text.Lazy                as LT

styleSheet :: LT.Text
styleSheet = render mnetStyle

headerFontFamily :: Css
headerFontFamily = fontFamily ["Inconsolata", "Consolas"] [monospace]

mnetStyle :: Css
mnetStyle = do
    body ? do
        maxWidth (em 50)
        fontFamily ["Verdana", "Geneva", "Helvetica"] [sansSerif]
        fontSize (em 0.9)
    h1 ? do
        headerFontFamily
        background yellow
        fontSize (em 2)
    h2 ? do
        headerFontFamily
        textDecoration underline
    h3 ? do
        headerFontFamily
        background yellow
    img ? do
        float floatRight
        border ridge (px 2) black
        margin 0 0 (em 1) (em 1)
        maxWidth (pct 40)
    div # byClass "announcement" ? do
        "clear" -: "both"
        height (pct 100)
        overflow hidden
        paddingLeft (em 1)
        maxWidth (em 50)
    span ? do
        "clear" -: "both"
