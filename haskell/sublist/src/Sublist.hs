module Sublist (Sublist(..), sublist) where

data Sublist = Equal | Sublist | Superlist | Unequal deriving (Eq, Show, Ord, Enum, Bounded)

sublist :: Eq a => [a] -> [a] -> Sublist
sublist xs ys
    | xs == ys = Equal
    | xs `elem` sublists (length xs) (length ys) ys = Sublist
    | ys `elem` sublists (length ys) (length xs) xs = Superlist
    | otherwise = Unequal

sublists :: Int -> Int -> [a] -> [[a]]
sublists min max xs = concatMap (sublistLength xs) [min..max]

sublistLength :: [a] -> Int -> [[a]]
sublistLength _ 0 = [[]]
sublistLength [] _ = []
sublistLength s@(_:xs) n
    | n <= length s = take n s : sublistLength xs n
    | otherwise = []