module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

import Data.Map (Map, findWithDefault, fromList)
import Data.List.Split (chunksOf)
import Data.List (transpose, sort)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

type Garden = Map String [Plant]

defaultGarden :: String -> Garden
defaultGarden = garden students
    where students = ["Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"]

garden :: [String] -> String -> Garden
garden students plants = fromList $ zip (sort students) (transform plants)
    where
        transform plants = map (map translatePlant . concat) $ transpose $ map (chunksOf 2) $ lines plants
        translatePlant x | x == 'V' = Violets | x == 'R' = Radishes | x == 'C' = Clover | x == 'G' = Grass

lookupPlants :: String -> Garden -> [Plant]
lookupPlants = findWithDefault []