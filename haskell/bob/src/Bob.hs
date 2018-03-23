module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor [] = "Fine. Be that way!"
responseFor str
    | blank = "Fine. Be that way!"
    | crazy = "Calm down, I know what I'm doing!"
    | shouting = "Whoa, chill out!"
    | question = "Sure."
    | otherwise = "Whatever."
    where   removeSpaces = filter (not . isSpace)
            allAlphaIsUpper = all isUpper . filter isAlpha
            shouting = any isAlpha str && allAlphaIsUpper str
            question = last (removeSpaces str) == '?'
            crazy = shouting && question
            blank = all isSpace str