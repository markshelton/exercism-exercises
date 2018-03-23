module Raindrops (convert) where

import Data.Map (fromList, findWithDefault, keys)

convert :: Int -> String
convert n
    | n <= 0 = show n
    | noFactors = show n
    | otherwise = concatMap applyRule $ factor n [1..n]
    where
        applyRule x = findWithDefault "" x rules
        noFactors = null $ factor n $ keys rules
        rules = fromList [(3, "Pling"), (5, "Plang"), (7, "Plong")]
        factor n = filter (\i -> n `mod` i == 0)