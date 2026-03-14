{-# LANGUAGE OverloadedStrings #-}
-- | Generates a preview HTML file for inspecting the report styling.
-- Build and run with: stack run preview-exe < preview/mock-report.json
-- Then open preview/report-preview.html in your browser.
module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           HTMLRenderer
import           Report
import           System.Exit          (die)

renderSection :: ReportSection -> String
renderSection section = case listings section of
    Left errMsg -> errorSectionToHtml (title section) errMsg
    Right ls    -> sectionToHtml (title section) ls

main :: IO ()
main = do
    input <- BSL.getContents
    report <- case eitherDecode input of
        Left err -> die $ "Failed to parse report JSON: " <> err
        Right r  -> pure r
    let sectionsHtml = concatMap renderSection (reportSections report)
        fullHtml     = sectionsToHtml "M.net-päivystäjä - uusia ilmoituksia" sectionsHtml
    putStrLn fullHtml
