module RotationalCipher (rotate) where

import Data.Char (ord, chr, isAlpha, isUpper, toLower, toUpper)

rotate :: Int -> String -> String
rotate n = map (translate n)

translate :: Int -> Char -> Char
translate n x
    | isAlpha x && isUpper x = toUpper x'
    | isAlpha x = x'
    | otherwise = x
    where 
        x' = cycle alphabet !! ix'
        ix' = ix + n
        ix = ord (toLower x) - ord 'a'
        alphabet = ['a'..'z']
