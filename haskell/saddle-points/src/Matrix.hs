module Matrix (saddlePoints) where

import Data.Array

type Index = (Int, Int)
type Matrix a = Array Index a

saddlePoints :: Ord a => Matrix a -> [Index]
saddlePoints mx = fmap fst $ filter checkPoint (assocs mx)
    where checkPoint ((r,c),x) = all (<=x) (getRow r mx) && all (>=x) (getCol c mx)

getRow :: Int -> Matrix a -> [a]
getRow r mx = fmap snd $ filter ((==r) . fst . fst) $ assocs mx

getCol :: Int -> Matrix a -> [a]
getCol c mx = fmap snd $ filter ((==c) . snd . fst) $ assocs mx
