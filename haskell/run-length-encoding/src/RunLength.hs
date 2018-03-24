module RunLength (decode, encode) where

import Data.Char (isNumber)
import Data.List (group)
import Data.Text (Text, pack, unpack, split)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

readInt :: Text -> Int
readInt t = fromMaybe 1 $ readMaybe $ unpack t

getNumbers :: String -> [Int]
getNumbers s = map readInt $ split (not . isNumber) $ pack s

getChars :: String -> String
getChars = filter (not . isNumber)

convert :: [(Int, Char)] -> [String]
convert [] = []
convert ((n, x):xs) = replicate n x : convert xs

decode :: String -> String
decode t = concat $ convert $ zip (getNumbers t) (getChars t)

countChars :: String -> String
countChars s
    | length s == 1 = [head s]
    | otherwise = show (length s) ++ [head s]

encode :: String -> String
encode t = concatMap countChars $ group t
