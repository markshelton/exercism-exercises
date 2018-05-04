module Spiral (spiral) where

import Data.List (foldl')
import Data.Matrix (Matrix, zero, toLists, setElem)

data Direction = R | D | L | U deriving (Eq, Show, Enum, Bounded)
type Position = (Int, Int)
type State = (Matrix Int, Position, Direction)

next :: (Eq a, Enum a, Bounded a) => a -> a
next e 
    | e == maxBound = minBound
    | otherwise = succ e

spiral :: Int -> [[Int]]
spiral size = toLists . place size $ generate size

generate :: Int -> [[Int]]
generate size = generate' [1..size*size] size size

generate' :: [Int] -> Int -> Int -> [[Int]]
generate' [] _ _ = []
generate' xs n prev = take n xs : generate' (drop n xs) n' n
    where n' = if n == prev then n - 1 else n

place :: Int -> [[Int]] -> Matrix Int
place size = fst' . foldl' placeEdge (initial size)
    where fst' (a,_,_) = a

initial :: Int -> State
initial size = (zero size size,(1,1), R)

placeEdge :: State -> [Int] -> State
placeEdge state [] = state
placeEdge (mx,pos,dir) [x] = placeEdge (setElem x pos mx, move pos (next dir), next dir) []
placeEdge (mx,pos,dir) (x:xs) = placeEdge (setElem x pos mx, move pos dir, dir) xs  

move :: Position -> Direction -> Position
move (r,c) R = (r,c+1)
move (r,c) D = (r+1,c) 
move (r,c) L = (r,c-1)
move (r,c) U = (r-1,c)