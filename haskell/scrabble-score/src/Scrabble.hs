module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)

scoreLetter :: Char -> Integer
scoreLetter letter
    | x `elem` "AEIOULNRST" = 1
    | x `elem` "DG" = 2
    | x `elem` "BCMP" = 3
    | x `elem` "FHVWY" = 4
    | x `elem` "K" = 5
    | x `elem` "JX" = 8
    | x `elem` "QZ" = 10
    | otherwise = 0
    where x = toUpper letter

scoreWord :: String -> Integer
scoreWord word = sum $ map scoreLetter word
