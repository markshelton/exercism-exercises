module Phone (number) where

import Data.Char (isNumber)

number :: String -> Maybe String
number s = checkFormat $ filter isNumber s

checkFormat :: String -> Maybe String
checkFormat s
    | length s == 10 = checkDigits s
    | length s == 11 && head s == '1'  = checkDigits $ tail s
    | otherwise = Nothing

checkDigits :: String -> Maybe String
checkDigits ('0':_) = Nothing
checkDigits ('1':_) = Nothing
checkDigits (_:_:_:'0':_) = Nothing
checkDigits (_:_:_:'1':_) = Nothing
checkDigits s = Just s