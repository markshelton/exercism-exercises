module Isogram (isIsogram) where

import Data.Char (isAlpha, toLower)

isIsogram :: String -> Bool
isIsogram xs = (alpha_len xs) == (alpha_len $ uniqueLower xs)
    where
        alpha_len = length . filter isAlpha
        unique [] = []
        unique (x:xs) = x : unique (filter (/=x) xs)
        uniqueLower xs = unique $ map toLower xs