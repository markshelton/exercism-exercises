module Grains (square, total) where

import Data.Maybe

square :: Integer -> Maybe Integer
square n
    | n <= 0 = Nothing
    | n > 64 = Nothing
    | otherwise = Just (2 ^ (n-1))

total :: Integer
total = sum $ map (fromMaybe 0) squares
    where   squares = map square [1..64]
