module Squares (difference, squareOfSums, sumOfSquares) where

difference :: Integral a => a -> a
difference n = squareOfSums n - sumOfSquares n

square :: Integral a => a -> a
square n = n * n

squareOfSums :: Integral a => a -> a
squareOfSums n = square $ sum [1..n]

sumOfSquares :: Integral a => a -> a
sumOfSquares n = sum $ map square [1..n]
