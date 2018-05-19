module RailFenceCipher (encode, decode) where

import Data.Matrix (Matrix)

import Data.Ord (comparing)
import Data.List
import Data.List.Split
import Data.Maybe (mapMaybe)
import qualified Data.Matrix as M (zero, toLists, setElem, ncols, transpose, toList)

data Direction = Up | Down deriving (Eq, Show)
type Position = (Int, Int)
type State = (Matrix Int, Direction, Position)

encode :: Int -> String -> String
encode n xs =
    let 
        xs' = concat . words $ xs
        rail = makeRail n xs'
        encode' = map . (!!)
    in encode' xs' rail

decode :: Int -> String -> String
decode n xs = 
    let 
        xs' = concat . words $ xs
        rail = makeRail n xs'
        decode' xs = map ((xs !!) . fst) . sortBy (comparing snd) . zip [0..]
    in decode' xs' rail

makeRail :: Int -> String -> [Int]
makeRail n xs = makeRail' (enumFromTo 1 (length xs)) (initial n (length xs)) 

makeRail' :: [Int] -> State -> [Int]
makeRail' [] (mx,_,_) = makeList mx
makeRail' (x:xs) (mx,dir,pos@(a,b)) = makeRail' xs (mx',dir',pos')
    where 
        mx' = M.setElem x pos mx
        dir' = if a /= 1 && (b == 1 || b == M.ncols mx) then swap dir else dir
        pos' = move dir' pos

makeList :: Matrix Int -> [Int]
makeList = filter (-1 /=) . map (subtract 1) . M.toList . M.transpose
        
initial :: Int -> Int -> State
initial rails msgLength = (M.zero msgLength rails,Up,(1,1))

swap :: Direction -> Direction
swap Down = Up
swap Up = Down

move :: Direction -> Position -> Position
move Up (a,b) = (a+1,b+1)
move Down (a,b) = (a+1,b-1)