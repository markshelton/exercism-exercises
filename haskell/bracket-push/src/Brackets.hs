module Brackets (arePaired) where

openingBrackets = "[{("
closingBrackets = "]})"
brackets = openingBrackets ++ closingBrackets

bracketPair :: Char -> Char
bracketPair ']' = '['
bracketPair '}' = '{'
bracketPair ')' = '('

arePaired :: String -> Bool
arePaired xs = arePaired' [] $ onlyBrackets xs

onlyBrackets :: String -> String
onlyBrackets = filter (flip elem brackets)

arePaired' :: String -> String -> Bool
arePaired' [] [] = True
arePaired' _ [] = False
arePaired' [] (x:xs)
    | x `elem` openingBrackets = arePaired' [x] xs
    | otherwise = False
arePaired' stack@(y:ys) (x:xs)
    | x `elem` openingBrackets = arePaired' (x:stack) xs
    | x `elem` closingBrackets = if bracketPair x == y then arePaired' ys xs else False
    | otherwise = arePaired' stack xs