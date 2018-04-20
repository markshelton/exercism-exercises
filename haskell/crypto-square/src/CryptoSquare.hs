module CryptoSquare (encode) where

import Data.Char (toLower, isAlpha, isNumber)
import Data.Maybe (mapMaybe)
import Data.List (transpose)
import Data.List.Split (chunksOf)

encode :: String -> String
encode = unwords . makeRect . mapMaybe normalize

makeRect :: String -> [String]
makeRect xs = transpose . (chunksOf c) $ xs
    where 
        (r,c) = minimum [ (r,c) | 
                c <- [1..length xs],
                r <- [1..length xs],
                c >=r, c-r <= 1, c*r >= length xs
            ]

normalize :: Char -> Maybe Char
normalize x
    | isAlpha x = Just x'
    | isNumber x = Just x'
    | otherwise = Nothing
        where x' = toLower x