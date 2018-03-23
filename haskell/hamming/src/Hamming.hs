module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
    | equalLength = Just $ count diff (zip xs ys)
    | otherwise = Nothing
    where   equalLength = length xs == length ys
            diff (x, y) = x /= y
            count pred = length . filter pred 