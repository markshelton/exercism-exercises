module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

type Coordinates = (Integer, Integer)
data Bearing = North | East | South | West deriving (Eq, Show, Enum, Bounded)
data Robot = Robot { bearing :: Bearing, coordinates :: Coordinates} deriving (Eq, Show)

-- bearing :: Robot -> Bearing
-- bearing robot = error "You need to implement this function."

-- coordinates :: Robot -> (Integer, Integer)
-- coordinates robot = error "You need to implement this function."

mkRobot :: Bearing -> Coordinates -> Robot
mkRobot = Robot

simulate :: Robot -> String -> Robot
simulate robot instructions = foldr act robot $ reverse instructions

act :: Char -> Robot -> Robot
act 'R' (Robot direction (x, y)) = Robot (turnRight direction) (x, y)
act 'L' (Robot direction (x, y)) = Robot (turnLeft direction) (x, y)
act 'A' (Robot direction (x, y)) = Robot direction (advance direction (x, y))

advance :: Bearing -> Coordinates -> Coordinates
advance North (x, y) = (x, y+1) 
advance East (x, y) = (x+1, y)
advance West (x, y) = (x-1, y)
advance South (x, y) = (x, y-1)

turnLeft :: Bearing -> Bearing
turnLeft = prev

turnRight :: Bearing -> Bearing
turnRight = next

next :: (Eq a, Bounded a, Enum a) => a -> a
next a = if a == maxBound then minBound else succ a

prev :: (Eq a, Bounded a, Enum a) => a -> a
prev a = if a == minBound then maxBound else pred a