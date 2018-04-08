module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V (length, head, fromList, toList, concat, map, empty, (!)) 
import Data.List.Split (chunksOf)
import qualified Data.List as L (transpose)

type Matrix a = Vector (Vector a)

cols :: Matrix a -> Int
cols matrix
    | null matrix = 0
    | otherwise = V.length $ V.head matrix

rows :: Matrix a -> Int
rows = V.length

shape :: Matrix a -> (Int, Int)
shape matrix = (rows matrix, cols matrix)

column :: Int -> Matrix a -> Vector a
column x = V.map (V.! x) 

row :: Int -> Matrix a -> Vector a
row = flip (V.!)

flatten :: Matrix a -> Vector a
flatten = V.concat . V.toList

fromList :: [[a]] -> Matrix a
fromList = V.fromList . map V.fromList

toList :: Matrix a -> [[a]]
toList = V.toList . V.map V.toList

fromString :: Read a => String -> Matrix a
fromString = fromList . map (map read . words) . lines

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (_, cs) matrix = fromList $ chunksOf cs $ V.toList $ flatten matrix

transpose :: Matrix a -> Matrix a
transpose = fromList . L.transpose . toList