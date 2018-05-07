module Transpose (transpose) where

transpose :: [String] -> [String]
transpose xss = trim (makeCutoffs xss) . transpose' . padLines $ xss

padLines :: [String] -> [String]
padLines xss = map (rpad $ maxLength xss) xss
    where 
        maxLength = maximum . map length
        rpad n xs = take n (xs ++ repeat ' ')

transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' ([]:xss) = transpose' xss
transpose' ((x:xs):xss) = (x: [h | (h:_) <- xss]) : transpose' (xs: [t | (_:t) <- xss])

makeCutoffs :: [String] -> [Int]
makeCutoffs = makeCutoffs' . map length

makeCutoffs' :: [Int] -> [Int]
makeCutoffs' [] = []
makeCutoffs' s@(x:xs) = maximum s : makeCutoffs' xs

trim :: [Int] -> [String] -> [String]
trim cutoffs xss = map (foldr trim' []) $ zippy cutoffs xss

zippy :: [Int] -> [String] -> [[(Int, Int, Char)]]
zippy cutoffs xss = map (\x -> zip3 cutoffs (repeat $ fst x) (snd x)) $ zip [1..] xss

trim' :: (Int, Int, Char) -> String -> String
trim' (a,b,c) acc = if b > a then acc else c : acc