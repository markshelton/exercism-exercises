module Roman (numerals) where

import qualified Data.Map as Map (Map, fromList, keys, lookup)

table = Map.fromList 
    [ (1000, "M"), (900, "CM"), (500, "D"), (400, "CD")
    , (100, "C"), (90, "XC"), (50, "L"), (40, "XL")
    , (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I") ]

numerals :: Integer -> Maybe String
numerals x = fmap concat . mapM (`Map.lookup` table) $ translate x

translate :: Integer -> [Integer]
translate 0 = []
translate x = getValue x : translate (x - getValue x) 

getValue :: Integer -> Integer
getValue x = maximum $ filter (<=x) (Map.keys table)
