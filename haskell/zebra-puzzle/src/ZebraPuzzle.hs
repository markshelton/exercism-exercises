module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (foldM)
import Data.Maybe (fromJust)
import Data.List ((\\), find)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese deriving (Eq, Show, Enum, Bounded)
data Colour = Red | Green | Ivory | Yellow | Blue deriving (Eq, Show, Enum, Bounded)
data Smoke = LuckyStrike | Parliaments | Chesterfields | Kools | OldGold deriving (Eq, Show, Enum, Bounded)
data Drink = Coffee | Tea | Milk | OrangeJuice | Water deriving (Eq, Show, Enum, Bounded)
data Pet = Snails | Fox | Horse | Dog | Zebra deriving (Eq, Show, Enum, Bounded)
data Location = FarLeft | InnerLeft | Middle | InnerRight | FarRight deriving (Eq, Show, Enum, Bounded)
data House = House { resident :: Resident, colour :: Colour, smoke :: Smoke, drink :: Drink, pet :: Pet, location :: Location } deriving (Eq, Show)
data Solution = Solution { waterDrinker :: Resident, zebraOwner :: Resident } deriving (Eq, Show)

is :: Eq a => (House -> a) -> a -> House -> Bool
(trait `is` value) house  = trait house == value

checkHouse :: House -> Bool
checkHouse house = and [
  resident `is` Englishman <=> colour `is` Red,
  resident `is` Spaniard <=> pet `is` Dog,
  drink `is` Coffee <=> colour `is` Green,
  resident `is` Ukrainian <=> drink `is` Tea,
  smoke `is` OldGold <=> pet `is` Snails,
  smoke `is` Kools <=> colour `is` Yellow,
  drink `is` Milk <=> location `is` Middle,
  resident `is` Norwegian <=> location `is` FarLeft,
  smoke `is` LuckyStrike <=> drink `is` OrangeJuice,
  smoke `is` Parliaments <=> resident `is` Japanese ]
  where 
    infix 4 <=>
    (<=>) :: (House -> Bool) -> (House -> Bool) -> Bool
    p <=> q = p house == q house

nextSolution :: [House] -> Int -> [[House]]
nextSolution solution _ = [ house: solution | house <- newHouses solution, checkHouse house ]
  
newHouses :: [House] -> [House]
newHouses solution = House <$> new resident  <*> new colour  <*> new smoke <*> new drink <*> new pet <*> new location
  where new trait = [minBound ..] \\ map trait solution

prelimSolutions :: [[House]]
prelimSolutions = map reverse $ foldM nextSolution [] [1..size]
    where size = length [(minBound :: Resident)..]

checkSolution :: [House] -> Bool
checkSolution solution = and [ 
  colour `is` Green `rightOf` colour `is` Ivory,
  smoke `is` Chesterfields `nextTo` pet `is` Fox,
  smoke `is` Kools `nextTo` pet `is` Horse,
  colour `is` Blue `nextTo` resident `is` Norwegian ]
  where
    infix 4 `nextTo`, `leftOf`, `rightOf`
    nextTo, leftOf, rightOf :: (House -> Bool) -> (House -> Bool) -> Bool
    nextTo p q = leftOf p q || rightOf p q
    leftOf = leftOf' solution
    rightOf = flip leftOf

leftOf' :: [House] -> (House -> Bool) -> (House -> Bool) -> Bool
leftOf' solution p q = case pLocation of
  FarRight -> False
  _ -> q $ fromJust $ find (location `is` succ pLocation) solution
  where pLocation = location $ fromJust $ find p solution

solve :: Solution
solve = Solution { 
    waterDrinker = resident $ fromJust $ find (drink `is` Water) solution, 
    zebraOwner = resident $ fromJust $ find (pet `is` Zebra) solution 
  }
  where solution = fromJust $ find checkSolution prelimSolutions