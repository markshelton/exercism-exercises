module PigLatin (translate) where

import Data.Tuple (swap)

data Syllable = Vowel | Consonant String deriving (Eq, Show)

vowels = ['a','e','i','o','u']
vowelsInner = 'y':vowels

translate :: String -> String
translate = unwords . map translateWord . words

translateWord :: String -> String
translateWord xs = case checkStart xs of
    Nothing -> xs
    Just Vowel -> xs ++ "ay"
    Just (Consonant ys) -> ys ++ "ay"

checkStart :: String -> Maybe Syllable
checkStart [] = Nothing
checkStart [a] = Nothing
checkStart s@(a:b:xs)
    | a `elem` vowels = Just Vowel
    | [a,b] `elem` ["xr", "yt"] = Just Vowel
    | otherwise = Just (Consonant s')
    where 
        s' = concatTuple . swap $ checkConsonant' s
        checkConsonant' s = checkConsonant s ("","")
        concatTuple (x,y) = concat (x : [y])

checkConsonant :: String -> (String, String) -> (String, String)
checkConsonant [] parts = parts
checkConsonant [a] parts = parts
checkConsonant (a:b:xs) (start, rest)
    | [a,b] == "qu" = checkConsonant xs (start ++ [a,b], rest)
    | a `elem` vowels = (start, a:b:xs)
    | b `elem` vowelsInner = (start ++ [a], b:xs)
    | otherwise = checkConsonant (b:xs) (start ++ [a], rest)
