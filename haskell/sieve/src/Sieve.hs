module Sieve (primesUpTo) where

import Data.List

primesUpTo :: Integer -> [Integer]
primesUpTo max = removeNonPrimes max [2..max] []

removeNonPrimes :: Integer -> [Integer] -> [Integer] -> [Integer]
removeNonPrimes _ [] acc = acc
removeNonPrimes max s@(x:xs) acc = x : removeNonPrimes max (xs \\ multiples x) acc
    where multiples a = takeWhile (<=max) . map (a *) $ [2..]