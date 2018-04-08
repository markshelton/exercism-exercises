module House (rhyme) where

import Data.List (intercalate)

line :: Int -> (String, String) 
line 0 = ("lay in", "house that Jack built" )
line 1 = ("ate", "malt") 
line 2 = ("killed", "rat")
line 3 = ("worried", "cat")
line 4 = ("tossed", "dog")
line 5 = ("milked", "cow with the crumpled horn")
line 6 = ("kissed", "maiden all forlorn")
line 7 = ("married", "man all tattered and torn")
line 8 = ("woke", "priest all shaven and shorn")
line 9 = ("kept", "rooster that crowed in the morn")
line 10 = ("belonged to", "farmer sowing his corn")
line 11 = ("", "horse and the hound and the horn")

repeated :: Int -> [String]
repeated 0 = [".\n"]
repeated x = ("\n" ++ "that " ++ fst (line (x-1)) ++ " the " ++ snd (line (x-1))) : repeated (x-1)

verse :: Int -> String
verse x = "This is the " ++ snd (line x) ++ concat (repeated x)

rhyme :: String
rhyme = intercalate "\n" $ map verse [0..11]