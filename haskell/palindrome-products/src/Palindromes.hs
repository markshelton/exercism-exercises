module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (find)
import Data.Maybe (fromJust)
import Control.Monad (replicateM)

largestPalindrome :: Integer -> Integer -> (Integer, [(Integer, Integer)])
largestPalindrome = getPalindromeProduct reverse

smallestPalindrome :: Integer -> Integer -> (Integer, [(Integer, Integer)])
smallestPalindrome = getPalindromeProduct id

getPalindromeProduct :: ([Integer] -> [Integer]) -> Integer -> Integer -> (Integer, [(Integer, Integer)])
getPalindromeProduct orderFunc min max = 
    let 
        palindromes = orderFunc $ makePalindromes min max
        factors = getFactorsInRange min max
        hasValidFactors = not . null . factors
        palindrome = fromJust $ find hasValidFactors palindromes
    in (palindrome, factors palindrome)

getFactorsInRange :: Integer -> Integer -> Integer -> [(Integer, Integer)]
getFactorsInRange min max n = [ (i,d) | 
    i <- [1..floor (sqrt (fromIntegral n))], 
    (d,0) <- [divMod n i], i <= d, i >= min, d <= max ]
                        
makePalindromes :: Integer -> Integer -> [Integer]
makePalindromes min max = concatMap makePalindromes' [minLength..maxLength]
    where 
        numDigits = length . show . fromInteger
        maxLength = numDigits (max*max)
        minLength = numDigits (min*min)

makePalindromes' :: Int -> [Integer]
makePalindromes' = map read . filter ((/='0') . head) . palindrome

palindrome :: Int -> [String]
palindrome n
    | n < 0  = []
    | even n = map (\front -> front ++ reverse front) fronts
    | odd n  = map (\front -> front ++ tail (reverse front)) fronts
    where fronts = replicateM (div (n + 1) 2) ['0'..'9']