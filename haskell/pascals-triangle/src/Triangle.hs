module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n = map row [1..n]

row :: Int -> [Integer]
row 0 = []
row 1 = [1]
row x = zipWith (+) (0 : row (x - 1)) (row (x -1) ++ [0])

