module ETL (transform) where

import Data.Char (toLower)
import Data.Map (Map, fromList, toList)

tx :: (a, String) -> [(Char, a)] -> [(Char, a)]
tx (_, []) acc = acc
tx (i, x:xs) acc = (toLower x, i) : tx (i, xs) acc

transform :: Map a String -> Map Char a
transform legacyData = fromList $ foldr tx [] (toList legacyData)