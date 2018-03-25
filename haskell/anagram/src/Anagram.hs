module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter (anagram xs)

anagram :: String -> String -> Bool
anagram xs ys
    | lower xs == lower ys = False
    | ordered xs == ordered ys = True
    | otherwise = False
    where 
        lower = map toLower
        ordered = sort . lower