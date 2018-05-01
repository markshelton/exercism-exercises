module OCR (convert) where

import Data.List (intercalate, transpose)
import Data.List.Split (chunksOf)

convert :: String -> String
convert = intercalate "," . recognizeDigits . chunksOf 4 . lines

recognizeDigits :: [[String]] -> [String]
recognizeDigits = map (concatMap recognizeDigit . transpose . map (chunksOf 3))

recognizeDigit :: [String] -> String 
recognizeDigit [ " _ ", "| |", "|_|","   "] = show 0
recognizeDigit [ "   ", "  |", "  |","   "] = show 1
recognizeDigit [ " _ ", " _|", "|_ ","   "] = show 2
recognizeDigit [ " _ ", " _|", " _|","   "] = show 3
recognizeDigit [ "   ", "|_|", "  |","   "] = show 4
recognizeDigit [ " _ ", "|_ ", " _|","   "] = show 5
recognizeDigit [ " _ ", "|_ ", "|_|","   "] = show 6
recognizeDigit [ " _ ", "  |", "  |","   "] = show 7
recognizeDigit [ " _ ", "|_|", "|_|","   "] = show 8
recognizeDigit [ " _ ", "|_|", " _|","   "] = show 9
recognizeDigit _ = "?"