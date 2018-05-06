module Frequency (frequency) where

import Data.Map (Map)
import Data.Text (Text)

import Data.Char (isLetter, toLower)
import Control.Parallel.Strategies (using, parListChunk, rseq)
import qualified Data.Map as M (empty, insertWith, unionsWith)
import qualified Data.Text as T (foldr)

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts = M.unionsWith (+) frequencies
    where 
        chunks = quot (length texts) nWorkers
        strategy = parListChunk chunks rseq
        frequencies = map textFreq texts `using` strategy

textFreq :: Text -> Map Char Int
textFreq = T.foldr updateFreq M.empty

updateFreq :: Char -> Map Char Int -> Map Char Int
updateFreq x acc
    | isLetter x = M.insertWith (+) (toLower x) 1 acc
    | otherwise = acc