module ProteinTranslation(proteins) where

import Data.Maybe (isNothing, fromJust)
import Data.List (elemIndex)
import Data.List.Split (chunksOf)

handleStop :: [String] -> Maybe [String]
handleStop proteins
    | isNothing findStop = Just proteins
    | otherwise = Just $ take (fromJust findStop) proteins
    where findStop = elemIndex "STOP" proteins

proteins :: String -> Maybe [String]
proteins s = handleStop . map translate $ chunksOf 3 s

translate :: String -> String
translate "AUG" = "Methionine"
translate "UUU" = "Phenylalanine"
translate "UUC" = "Phenylalanine"
translate "UUA" = "Leucine"
translate "UUG" = "Leucine"
translate "UCU" = "Serine"
translate "UCC" = "Serine"
translate "UCA" = "Serine"
translate "UCG" = "Serine"
translate "UAU" = "Tyrosine"
translate "UAC" = "Tyrosine"
translate "UGU" = "Cysteine"
translate "UGC" = "Cysteine"
translate "UGG" = "Tryptophan"
translate "UAA" = "STOP"
translate "UAG" = "STOP"
translate "UGA" = "STOP"