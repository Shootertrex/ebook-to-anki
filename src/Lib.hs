{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( ebookToAnki
    ) where

import Data.List.Split(splitOn)
import Data.List(nub)
import Data.Text(pack, unpack, replace)

replaceT :: String -> String -> String -> String
replaceT pattern replacement input = unpack $ replace (pack pattern) (pack replacement) (pack input)

deleteFirst:: Eq a => a -> [a] -> [a]
deleteFirst _ [] = []
deleteFirst ele (x:xs)
    | (ele == x) = xs
    | otherwise = x : deleteFirst ele xs

ebookToAnki :: String -> String -> [String]
ebookToAnki content series = do 
            let filtered = deleteFirst ""
                         . nub 
                         . splitOn "\n" 
                         . replaceT "、" ",\n" 
                         . replaceT "。" ".\n" 
                         . replaceT "！" "!\n" 
                         . replaceT "？" "?\n" 
                         . replaceT "。」" ".」" 
                         . replaceT "！」" "!」" 
                         $ replaceT "？」" "?」" content

            let count = length filtered
            let labels = map (\x -> series ++ "_" ++ (show x) ++ "\t") [1..count]
            zipWith (++) labels filtered 
          

-- 。 ？ 」 ！ 、
