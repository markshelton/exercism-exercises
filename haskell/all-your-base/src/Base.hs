module Base (Error(..), rebase) where

import Data.List (find)
import Data.Maybe (fromJust)

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase <= 1 = Left InvalidInputBase
    | outputBase <= 1 = Left InvalidOutputBase
    | any (invalidDigit inputBase) inputDigits = Left $ InvalidDigit $ fromJust $ find (invalidDigit inputBase) inputDigits
    | otherwise = Right $ convertFrom10 outputBase $ convertTo10 inputBase inputDigits

invalidDigit :: Integral a => a -> a -> Bool
invalidDigit inputBase digit
    | digit >= inputBase = True
    | digit < 0 = True
    | otherwise = False

convertTo10 :: Integral a => a -> [a] -> a
convertTo10 inputBase inputDigits = convertTo10' inputBase $ zip [0..] $ reverse inputDigits

convertTo10' :: Integral a => a -> [(Int, a)] -> a
convertTo10' inputBase [] = 0
convertTo10' inputBase ((i, x):xs) = x * inputBase ^ i + convertTo10' inputBase xs

convertFrom10 :: Integral a => a -> a -> [a]
convertFrom10 _ 0 = []
convertFrom10 outputBase x = convertFrom10 outputBase (x `div` outputBase) ++ [x `mod` outputBase]