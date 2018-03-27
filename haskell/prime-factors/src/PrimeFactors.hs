module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n = reverse $ prime 2 n []

prime :: Integer -> Integer -> [Integer] -> [Integer]
prime d 1 acc = acc
prime d x acc
    | x `mod` d == 0 = prime d (x `div` d) (d : acc)
    | otherwise = prime (d+1) x acc