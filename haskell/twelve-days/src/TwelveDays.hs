module TwelveDays (recite) where

interleave :: [a] -> [a] -> [a]
interleave (x:xs) ys = x : interleave ys xs
interleave [] ys = ys

recite :: Int -> Int -> [String]
recite start stop = map lyric [start..stop] 

lyric :: Int -> String
lyric x = intro x ++ concat (interleave (grammar x) (gifts x))

intro :: Int -> String
intro x = "On the " ++ day x ++ " day of Christmas my true love gave to me" 

gifts :: Int -> [String]
gifts x = reverse $ map gift [1..x]

grammar :: Int -> [String]
grammar x
    | x == 1 = ", " : ["."]
    | x >= 1 = replicate (x-1) ", " ++ [", and "] ++ ["."]
    | otherwise = []

day :: Int -> String
day 1 = "first"
day 2 = "second"
day 3 = "third"
day 4 = "fourth"
day 5 = "fifth"
day 6 = "sixth"
day 7 = "seventh"
day 8 = "eighth"
day 9 = "ninth"
day 10 = "tenth"
day 11 = "eleventh"
day 12 = "twelfth"

gift :: Int -> String
gift 1 = "a Partridge in a Pear Tree"
gift 2 = "two Turtle Doves"
gift 3 = "three French Hens"
gift 4 = "four Calling Birds"
gift 5 = "five Gold Rings"
gift 6 = "six Geese-a-Laying"
gift 7 = "seven Swans-a-Swimming"
gift 8 = "eight Maids-a-Milking"
gift 9 = "nine Ladies Dancing"
gift 10 = "ten Lords-a-Leaping"
gift 11 = "eleven Pipers Piping"
gift 12 = "twelve Drummers Drumming"