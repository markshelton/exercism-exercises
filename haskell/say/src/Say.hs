module Say (inEnglish) where

import Data.List.Split (chunksOf)
import Data.List (intercalate, intersperse, dropWhileEnd)
import Data.Char (digitToInt, isSpace)
import Data.Tuple (swap)
import Control.Monad.Extra ((&&^))

scales = ["","thousand","million","billion","trillion"]

inEnglish :: Integer -> Maybe String
inEnglish n
    | n < 0 = Nothing
    | n >= 10^12 = Nothing
    | otherwise = Just (inEnglish' n)

inEnglish' :: Integer -> String
inEnglish' = 
    let 
        makeChunks = map inEnglishChunk . decompose
        scaleChunks = reverse . zip scales
        checkZeros xs = if length xs == 1 then xs else filter ((/="zero") . snd) xs
        formatText = dropWhileEnd isSpace . unwords . map (concatTuple . swap)
    in  formatText . checkZeros . scaleChunks . makeChunks
    where concatTuple (x,y) = x ++ " " ++ y

inEnglishChunk :: Integer -> String
inEnglishChunk n = case n' of
    [0,0,c] -> translate c
    [0,1,c] -> translate (10+c)
    [0,b,0] -> translate (b*10)
    [0,b,c] -> translate (b*10) ++ "-" ++ translate c
    [a,1,c] -> translate a ++ " " ++ translate 100 ++ " " ++ translate (10+c)
    [a,0,0] -> translate a ++ " " ++ translate 100
    [a,b,0] -> translate a ++ " " ++ translate 100 ++ " " ++ translate (b*10)
    [a,0,c] -> translate a ++ " " ++ translate 100 ++ " " ++ translate c
    [a,b,c] -> translate a ++ " " ++ translate 100 ++ " " ++ translate (b*10) ++ "-" ++ translate c
    where 
        n' = map digitToInt $ lpad 3 $ show n
        lpad m xs = replicate (m - length ys) '0' ++ ys where ys = take m xs

decompose :: Integer -> [Integer]
decompose = map ((read :: String->Integer) . reverse) . chunksOf 3 . reverse . show

translate :: Int -> String
translate 0 = "zero"
translate 1 = "one"
translate 2 = "two"
translate 3 = "three"
translate 4 = "four"
translate 5 = "five"
translate 6 = "six"
translate 7 = "seven"
translate 8 = "eight"
translate 9 = "nine"
translate 10 = "ten"
translate 11 = "eleven"
translate 12 = "twelve"
translate 13 = "thirteen"
translate 14 = "fourteen"
translate 15 = "fifteen"
translate 16 = "sixteen"
translate 17 = "seventeen"
translate 18 = "eighteen"
translate 19 = "nineteen"
translate 20 = "twenty"
translate 30 = "thirty"
translate 40 = "forty"
translate 50 = "fifty"
translate 60 = "sixty"
translate 70 = "seventy"
translate 80 = "eighty"
translate 90 = "ninety"
translate 100 = "hundred"