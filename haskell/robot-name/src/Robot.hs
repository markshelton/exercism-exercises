module Robot (Robot, mkRobot, resetName, robotName) where

import System.Random (mkStdGen, randomRs)
import Data.Unique (newUnique, hashUnique)
import Data.IORef (IORef, newIORef, modifyIORef, readIORef)

newtype Robot = Robot { getName :: String } deriving (Eq, Show)

mkRobot :: IO (IORef Robot)
mkRobot = do 
    newName <- generateName
    newIORef (Robot newName)

resetName :: IORef Robot -> IO ()
resetName robot = do 
    newName <- generateName
    modifyIORef robot (\r -> r {getName = newName})

robotName :: IORef Robot -> IO String
robotName robot = do
    robot <- readIORef robot
    return (getName robot)

generateName :: IO String
generateName = do 
    unique <- newUnique
    let hash = hashUnique unique
    let gen = mkStdGen hash
    let alphas = take 2 $ randomRs ('A','Z') gen
    let numbers = take 3 $ randomRs ('0','9') gen
    let newName = alphas ++ numbers
    return newName
