module Minesweeper (annotate) where

import Data.Char (intToDigit)
import Data.List.Split (chunksOf)
import Data.Array (listArray, inRange, assocs)

type Index = (Int, Int)
type Bounds = (Int, Int)

annotate :: [String] -> [String]
annotate [] = []
annotate [""] = [""]
annotate xs = annotatedBoard
    where 
        annotatedBoard = writeBoard bounds countArray
        countArray = fmap (countMines inputArray) inputArray
        inputArray = readBoard bounds xs
        bounds = (length xs, length $ head xs)

writeBoard :: Bounds -> [Int] -> [String]
writeBoard (_,cols) = chunksOf cols . fmap writeCell

writeCell :: Int -> Char
writeCell 9 = '*'
writeCell 0 = ' '
writeCell x = intToDigit x

countMines :: [(Index,Int)] -> (Index,Int) -> Int
countMines _ (_,1) = 9
countMines as (i@(x,y),_) = sum [e | (i,e) <- as, inRange ((x-1,y-1),(x+1,y+1)) i]

readBoard :: Bounds -> [String] -> [(Index,Int)]
readBoard bounds = assocs . listArray ((1,1),bounds) . fmap readCell . concat

readCell :: Char -> Int
readCell '*' = 1
readCell _ = 0