module Beer (song) where

import Data.Char (toUpper)

song :: String
song = concatMap lyric $ reverse [0..99]

lyric :: Int -> String
lyric n = 
        capitalise (no_more n) ++ bottle n ++ "of beer on the wall, " ++
        no_more n ++ bottle n ++ "of beer.\n" ++
        take_it_down n ++ ", " ++
        no_more (n-1) ++ bottle (n-1) ++ "of beer on the wall.\n" ++ lyric_break n
    where 
        capitalise s = toUpper (head s) : tail s
        take_it_down n
            | n == 1 = "Take it down and pass it around"
            | n == 0 = "Go to the store and buy some more"
            | otherwise = "Take one down and pass it around"
        lyric_break n | n == 0 = "" | otherwise = "\n"
        bottle n | n == 1 = " bottle " | otherwise = " bottles "
        no_more n | n == -1 = "99" | n == 0 = "no more" | otherwise = show n