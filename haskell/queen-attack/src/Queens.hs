module Queens (boardString, canAttack) where

import Data.List.Split (chunksOf)

type Position = (Int, Int)

boardString :: Maybe Position -> Maybe Position -> String
boardString w b = formatString $ map (snd . placePieces w b) $ zip [0..63] (repeat '_')

formatString :: String -> String
formatString = unlines . map (unwords . map (: [])) . chunksOf 8

placePieces :: Maybe Position -> Maybe Position -> (Int, Char) -> (Int, Char)
placePieces w b = placePiece w 'W' . placePiece b 'B'

placePiece :: Maybe Position -> Char -> (Int, Char) -> (Int, Char)
placePiece Nothing _ z = z
placePiece (Just (a,b)) c (i,x)
    | a*8+b == i = (i,c)
    | otherwise = (i,x)

canAttack :: Position -> Position -> Bool
canAttack (wx, wy) (bx, by)
    | wx == bx = True
    | wy == by = True
    | (==1) . abs $ quot (bx-wx) (wy-by) = True
    | otherwise = False
