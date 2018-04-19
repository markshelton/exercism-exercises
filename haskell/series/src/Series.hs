module Series (slices) where

import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices n xs = subList n $ digits xs

digits :: String -> [Int]
digits = fmap digitToInt

subList :: Int -> [a] -> [[a]]
subList 0 _ = [[]]
subList _ [] = []
subList n s@(_:xs) 
    | n <= length s = take n s : subList n xs
    | otherwise = []