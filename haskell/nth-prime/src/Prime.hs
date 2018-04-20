module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
    | n <= 0 = Nothing
    | otherwise = Just $ findPrimes n

findPrimes :: Int -> Integer
findPrimes = findPrimes' [] 2

findPrimes' :: [Integer] -> Integer -> Int -> Integer
findPrimes' primes curr target
    | length primes == target = curr-1
    | any (\i -> curr `mod` i == 0) primes = findPrimes' primes (curr+1) target
    | otherwise = findPrimes' (curr:primes) (curr+1) target