module SecretHandshake (handshake) where

import Data.List (elemIndices, delete)

maybeReverse :: [String] -> [String]
maybeReverse s
    | "reverse" `elem` s = maybeReverse . reverse $ delete "reverse" s
    | otherwise = s

handshake :: Int -> [String]
handshake = maybeReverse . map translate . elemIndices 1 . convert

convert :: Int -> [Int]
convert 0 = []
convert x = x `mod` 2 : convert (x `div` 2)

translate :: Int -> String
translate 0 = "wink"
translate 1 = "double blink"
translate 2 = "close your eyes"
translate 3 = "jump"
translate 4 = "reverse"