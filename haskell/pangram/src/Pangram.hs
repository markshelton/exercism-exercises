module Pangram (isPangram) where

import Data.Char
import Data.List

isPangram :: String -> Bool
isPangram [] = False
isPangram text = 
    let alphabet = ['a'..'z']
        unique [] = []
        unique (x:xs) = x : unique (filter (/=x) xs)
        chars = filter isAlpha $ map toLower text
    in alphabet == sort (unique chars)
