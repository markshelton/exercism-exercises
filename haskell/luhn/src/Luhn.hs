module Luhn (isValid) where

import Data.Char

isValid :: String -> Bool
isValid xs = case tx of    
    [] -> False
    [x] -> False
    tx -> (==0) . flip mod 10 . sum $ tx
    where tx = transform xs

transform :: String -> [Int]
transform = reverse . zipWith (curry convert) [1..] . reverse . map digitToInt . filter isDigit

convert :: (Int,Int) -> Int
convert (i,x)
    | i `mod` 2 == 0 = if 2*x > 9 then 2*x - 9 else 2*x
    | otherwise = x