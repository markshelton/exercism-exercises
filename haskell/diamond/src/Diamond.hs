module Diamond (diamond) where

import Data.Char (ord, chr)

diamond :: Char -> Maybe [String]
diamond char
    | char `elem` ['A'..'Z'] = Just $ drawPattern char
    | otherwise = Nothing
    where 
        drawPattern char = map drawLine grid
        drawLine lineNo = map drawChar $ zip (repeat lineNo) grid
        drawChar (lineNo, pos)
            | pos == delta = charX
            | pos == width - delta = charX
            | otherwise = ' '
            where
                charX = chr $ ord char - delta
                delta = abs $ intVal - lineNo
        intVal = ord char - ord 'A'
        width = 2 * intVal
        grid = [0..width]    