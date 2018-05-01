module FoodChain (song) where

import Data.List (intercalate)

lyric :: Int -> (String, String)
lyric 0 = ("fly", "I don't know why she swallowed the fly. Perhaps she'll die.")
lyric 1 = ("spider", "It wriggled and jiggled and tickled inside her.")
lyric 2 = ("bird", "How absurd to swallow a bird!")
lyric 3 = ("cat", "Imagine that, to swallow a cat!")
lyric 4 = ("dog", "What a hog, to swallow a dog!")
lyric 5 = ("goat", "Just opened her throat and swallowed a goat!")
lyric 6 = ("cow", "I don't know how she swallowed a cow!")
lyric 7 = ("horse", "She's dead, of course!")

swallow :: Int -> [String]
swallow 0 = []
swallow 7 = []
swallow n = next_line : swallow (n-1)
    where next_line =   "She swallowed the "
                    ++  fst (lyric n)
                    ++  " to catch the "
                    ++  fst (lyric (n-1))
                    ++ case n-1 of
                        1 -> " that wriggled and jiggled and tickled inside her."
                        _ -> "."

verse :: Int -> String
verse n
    | n == 0 = intro
    | n == 7 = intro ++ swallow_lines
    | otherwise = intro ++ swallow_lines ++ outro
    where 
        intro = first_line ++ second_line
        first_line = "I know an old lady who swallowed a " ++ fst (lyric n) ++ ".\n"
        second_line = snd (lyric n)
        swallow_lines = "\n" ++ unlines (swallow n)
        outro = snd (lyric 0)
         

song :: String
song = intercalate "\n\n" . map verse $ [0..7]