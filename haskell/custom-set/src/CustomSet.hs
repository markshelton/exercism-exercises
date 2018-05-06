{-# LANGUAGE DeriveFoldable,DeriveFunctor,DeriveTraversable #-}

module CustomSet
  ( delete
  , difference
  , empty
  , fromList
  , insert
  , intersection
  , isDisjointFrom
  , isSubsetOf
  , member
  , null
  , size
  , toList
  , union
  ) where

import Prelude hiding (null)
import Data.List (sort)
import Data.Foldable (Foldable)
import Data.Functor (Functor)
import Data.Traversable (Traversable)

newtype CustomSet a = CustomSet { getSet :: [a] } deriving (Show, Foldable, Functor, Traversable)

instance Eq a => Eq (CustomSet a) where
  (==) = isEqual

--------

empty :: CustomSet a
empty = CustomSet []

fromList :: Eq a => [a] -> CustomSet a
fromList = foldr insert empty

toList :: CustomSet a -> [a]
toList = getSet

--------

add :: Eq a => a -> CustomSet a -> CustomSet a
add x set = CustomSet (x : toList set)

delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x = foldr (\a set -> if a /= x then add a set else set) empty

insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x set = if member x set then set else add x set

member :: Eq a => a -> CustomSet a -> Bool
member = elem

size :: CustomSet a -> Int
size = length

--------

union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union = foldr insert

intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection setA setB = foldr insert' empty (setA `union` setB)
  where 
    insert' i x = if check i setA setB then insert i x else x
    check x setA setB = (&&) (member x setA) (member x setB)

difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference setA = foldr (\b acc -> if b `elem` setA then delete b acc else acc) setA

--------

null :: Eq a => CustomSet a -> Bool
null = (==empty)

isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA = (==empty) . intersection setA

isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf setA = (==setA) . intersection setA

isEqual :: Eq a => CustomSet a -> CustomSet a -> Bool
isEqual setA setB = size (union setA setB) == size (intersection setA setB)

--------