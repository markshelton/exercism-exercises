module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = Empty | Node a (BST a) (BST a) deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty = Nothing
bstLeft (Node _ l _) = Just l

bstRight :: BST a -> Maybe (BST a)
bstRight Empty = Nothing
bstRight (Node _ _ r) = Just r

bstValue :: BST a -> Maybe a
bstValue Empty = Nothing
bstValue (Node x _ _) = Just x

empty :: BST a
empty = Empty

singleton :: a -> BST a
singleton x = Node x Empty Empty

insert :: Ord a => a -> BST a -> BST a
insert x Empty = singleton x
insert x (Node y l r)
    | x <= y = Node y (insert x l) r
    | otherwise = Node y l (insert x r)

fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) empty
    
toList :: BST a -> [a]
toList Empty = []
toList (Node x l r) = toList l ++ [x] ++ toList r