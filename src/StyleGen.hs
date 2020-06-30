{-# LANGUAGE OverloadedStrings #-}
module StyleGen (styleSheet) where
import           Prelude                 hiding ( div
                                                , span
                                                )
import           Clay

styleSheet = render mnetStyle

linkColor :: Color
linkColor = black

headerFonts = ["Inconsolata"]

mnetStyle :: Css
mnetStyle = do
    body ? do
        maxWidth (em 50)
        fontFamily ["Verdana", "Geneva", "Helvetica"] [sansSerif]
        fontSize (em 0.9)
    h1 ? do
        fontFamily headerFonts []
        background yellow
        fontSize (em 2)
    h2 ? do
        fontFamily headerFonts [monospace]
        background yellow
    img ? do
        float floatRight
        border ridge (px 2) black
        margin 0 0 (em 1) (em 1)
    a # visited ? color linkColor
    div # byClass "announcement" ? do
        "clear" -: "both"
        height (pct 100)
        overflow hidden
        paddingLeft (em 1)
    span ? do
        "clear" -: "both"
