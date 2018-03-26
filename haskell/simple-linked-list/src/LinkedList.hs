module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Empty | Node a (LinkedList a) deriving (Eq, Show)

datum :: LinkedList a -> a
datum (Node x xs) = x

fromList :: [a] -> LinkedList a
fromList = foldr Node Empty

isNil :: LinkedList a -> Bool
isNil Empty = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new = Node

next :: LinkedList a -> LinkedList a
next Empty = Empty
next (Node x xs) = xs

nil :: LinkedList a
nil = Empty

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = fromList . reverse . toList

toList :: LinkedList a -> [a]
toList Empty = []
toList (Node x xs) = x : toList xs