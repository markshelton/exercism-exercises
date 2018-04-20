module Atbash (decode, encode) where

import Data.Maybe (mapMaybe)
import Data.Char (ord, chr, isAlpha, isDigit, toLower)
import Data.List (unwords)
import Data.List.Split (chunksOf)

decode :: String -> String
decode = mapMaybe cipher

encode :: String -> String
encode = unwords . chunksOf 5 . decode

cipher :: Char -> Maybe Char
cipher x
    | isAlpha x = Just (translate x)
    | isDigit x = Just x
    | otherwise = Nothing

translate :: Char -> Char
translate x = chr (ord 'a' - ord (toLower x) + ord 'z')
