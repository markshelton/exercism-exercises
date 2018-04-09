module Triplet (isPythagorean, mkTriplet, pythagoreanTriplets) where

isPythagorean :: (Int, Int, Int) -> Bool
isPythagorean (a, b, c)
    | a^2 + b^2 == c^2 = True
    | a^2 + c^2 == b^2 = True
    | b^2 + c^2 == a^2 = True
    | otherwise = False

mkTriplet :: Int -> Int -> Int -> (Int, Int, Int)
mkTriplet a b c = (a, b, c)

pythagoreanTriplets :: Int -> Int -> [(Int, Int, Int)]
pythagoreanTriplets minFactor maxFactor = [ 
    (a,b,c) | 
        a <- [minFactor..maxFactor],
        b <- [minFactor..maxFactor],
        c <- [minFactor..maxFactor],
        a^2 + b^2 == c^2,
        a <= b, b <= c
    ]