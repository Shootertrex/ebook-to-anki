module Main where

import System.Environment
import Lib

main :: IO ()
main = do filePath <- head <$> getArgs
          seriesName <- last <$> getArgs
          contents <- readFile filePath
          let formatted = ebookToAnki contents seriesName
          writeFile ("./" ++ seriesName ++ ".txt") (unlines formatted)
