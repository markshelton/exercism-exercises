module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)


factorize :: Int -> [Int]
factorize x = filter (\f -> x `mod` f == 0) [1..x-1]

classify :: Int -> Maybe Classification
classify x
    | x <= 0 = Nothing
    | x == y = Just Perfect
    | x > y = Just Deficient
    | x < y = Just Abundant
    where y = sum $ factorize x