module Series (Error(..), largestProduct) where

import Data.Char (isNumber, digitToInt)
import Data.List (find, subsequences)
import Data.Maybe (fromJust)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
    | length digits < size || size < 0 = Left InvalidSpan
    | any (not . isNumber) digits = Left $ InvalidDigit $ fromJust $ find (not . isNumber) digits
    | otherwise = Right $ largestProduct' size $ map digitToInt digits

largestProduct' :: Int -> [Int] -> Integer
largestProduct' size digits = toInteger . maximum . map product $ subList size digits

subList :: Int -> [a] -> [[a]]
subList 0 _ = [[]]
subList _ [] = []
subList n s@(_:xs) 
    | n <= length s = take n s : subList n xs
    | otherwise = []