module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.List (elemIndices, delete)

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              | Unknown
              deriving (Eq, Show)

allergies :: Int -> [Allergen]
allergies = filter (/= Unknown) . map translate . elemIndices 1 . convert

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen = elem allergen . allergies

convert :: Int -> [Int]
convert 0 = []
convert x = x `mod` 2 : convert (x `div` 2)

translate :: Int -> Allergen
translate 0 = Eggs
translate 1 = Peanuts
translate 2 = Shellfish
translate 3 = Strawberries
translate 4 = Tomatoes
translate 5 = Chocolate
translate 6 = Pollen
translate 7 = Cats
translate _ = Unknown