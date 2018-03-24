module IsbnVerifier (isbn) where

import Data.Maybe (mapMaybe)
import Data.Char (isNumber, digitToInt)

isbn :: String -> Bool
isbn s
    | s == "" = False
    | not $ verifyDigits $ init s = False
    | not $ verifyCheck $ last s = False
    | otherwise = calculateSum s `mod` 11 == 0

verifyDigits :: String -> Bool
verifyDigits s = all verifyDigit s && (length (filter isNumber s) == 9)

verifyDigit :: Char -> Bool
verifyDigit x = isNumber x || x == '-'

verifyCheck :: Char -> Bool
verifyCheck x = isNumber x || x == 'X'

calculateSum :: String -> Int
calculateSum s = foldr getCheckValue 0 enumDigits
    where enumDigits = zip (reverse [1..10]) (getDigits s)

getDigits :: String -> [Int]
getDigits = mapMaybe getDigitValue

getDigitValue :: Char -> Maybe Int
getDigitValue x
    | isNumber x = Just $ digitToInt x
    | x == 'X' = Just 10
    | otherwise = Nothing

getCheckValue :: (Int, Int) -> Int -> Int
getCheckValue (i, x) acc = x * i + acc