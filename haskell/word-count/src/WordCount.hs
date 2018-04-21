module WordCount (wordCount) where

import Data.Char (isPunctuation, isSpace, isAlphaNum, toLower)
import Data.List (group, sort, dropWhileEnd)
import Data.List.Split (wordsBy)
import Control.Applicative (liftA2)
import Control.Arrow ((&&&))

wordCount :: String -> [(String, Int)]
wordCount = map (head &&& length) . group . sort . filter (not . null) . map normalize . wordsBy isSeparator

isSeparator :: Char -> Bool
isSeparator = liftA2 (||) isSpace $ liftA2 (&&) isPunctuation (/='\'')

normalize :: String -> String
normalize = map toLower . dropWhileEnd (not . isAlphaNum) . dropWhile (not . isAlphaNum)