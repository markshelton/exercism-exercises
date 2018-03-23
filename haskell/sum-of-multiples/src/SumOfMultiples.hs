module SumOfMultiples (sumOfMultiples) where

divides :: [Integer] -> Integer -> Bool
divides factors x = any (\fac -> x `rem` fac == 0) factors

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum [x | x <- [1..limit-1], divides factors x]