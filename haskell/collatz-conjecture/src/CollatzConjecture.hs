module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
    | n <= 0 = Nothing
    | n == 1 = Just 0
    | otherwise = getSum [Just 1, collatz (next)]
    where   getSum = fmap sum . sequence
            next = if n `mod` 2 == 0 then n `div` 2 else n * 3 + 1