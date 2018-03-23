module DNA (nucleotideCounts) where

import Data.Map (Map, insertWith, fromList)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs = count initial xs
    where
        count map [] = Right map
        count map (x:xs)    | valid x = count (increment x map) xs
                            | otherwise = Left (x : " is not a valid base")
        initial = fromList $ zip nucleotides $ repeat 0
        valid x | x `elem` nucleotides = True | otherwise = False
        increment x = insertWith (+) x 1
        nucleotides = ['A', 'C', 'G', 'T']

        