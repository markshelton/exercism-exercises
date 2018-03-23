module Acronym (abbreviate) where

import Data.Char (isSpace, isUpper, isPunctuation, isAlpha, toUpper)
import Data.Maybe (catMaybes)

toAcronym :: String -> Char
toAcronym = toUpper . head

isBreak :: String -> Char -> Int -> Maybe Int
isBreak xs x i
    | i == 0 = Just i
    | isSpace x = Just (i + 1)
    | isPunctuation x && isAlpha next = Just (i + 1)
    | isUpper x && not (isUpper prev) = Just i
    | otherwise = Nothing
    where prev = xs!!(i-1); next = xs!!(i+1)

enumerate :: [a] -> [(Int, [a], a)]
enumerate xs = zip3 [0..] (repeat xs) xs

findBreaks :: String -> [Maybe Int]
findBreaks xs = foldl (\acc (i, xs, x) -> if isBreak xs x i `elem` acc then acc else acc ++ [isBreak xs x i]) [] (enumerate xs) 

abbreviate :: String -> String
abbreviate xs = map (toUpper . (\i -> xs !! i)) $ catMaybes (findBreaks xs)

