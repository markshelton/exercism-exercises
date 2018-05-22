module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef (IORef, newIORef, modifyIORef, readIORef)
import Control.Monad (join)

data Node a = Node { value :: a, prevNode :: Maybe (NodeRef a), nextNode :: Maybe (NodeRef a) }
data Deque a = Deque { firstNode :: Maybe (NodeRef a), lastNode :: Maybe (NodeRef a) }
data Direction = After | Before

type NodeRef a = IORef (Node a) 

mkDeque :: IO (IORef (Deque a))
mkDeque = newIORef Deque { firstNode = Nothing, lastNode = Nothing }

pop :: IORef (Deque a) -> IO (Maybe a)
pop deque = do
    rDeque <- readIORef deque
    remove (lastNode rDeque) deque
            
shift :: IORef (Deque a) -> IO (Maybe a)
shift deque = do
    rDeque <- readIORef deque
    remove (firstNode rDeque) deque

push :: IORef (Deque a) -> a -> IO ()
push deque x = do
    rDeque <- readIORef deque
    insert After (lastNode rDeque) deque x

unshift :: IORef (Deque a) -> a -> IO ()
unshift deque x = do
    rDeque <- readIORef deque
    insert Before (firstNode rDeque) deque x

---

insert :: Direction -> Maybe (NodeRef a) -> IORef (Deque a) -> a -> IO ()
insert _ Nothing = insertOnEmpty
insert Before (Just target) = insertBefore target
insert After (Just target) = insertAfter target

insertOnEmpty :: IORef (Deque a) -> a -> IO ()
insertOnEmpty deque x = do
    newNode <- newIORef Node { value = x, prevNode = Nothing, nextNode = Nothing }
    modifyIORef deque (\d -> d { firstNode = Just newNode, lastNode = Just newNode })

insertBefore :: NodeRef a -> IORef (Deque a) -> a -> IO ()
insertBefore target deque x = do
    rNode <- readIORef target
    newNode <- newIORef Node { value = x, prevNode = prevNode rNode, nextNode = Just target }
    modifyIORef target (\n -> n { prevNode = Just newNode })
    case prevNode rNode of
        Nothing -> modifyIORef deque (\d -> d { firstNode = Just newNode })
        Just pNode -> modifyIORef pNode (\n -> n { nextNode = Just newNode })

insertAfter :: NodeRef a -> IORef (Deque a) -> a -> IO ()
insertAfter target deque x = do
    rNode <- readIORef target
    newNode <- newIORef Node { value = x, prevNode = Just target, nextNode = nextNode rNode }
    modifyIORef target (\n -> n { nextNode = Just newNode })        
    case nextNode rNode of
        Nothing -> modifyIORef deque (\d -> d { lastNode = Just newNode })
        Just nNode -> modifyIORef nNode (\n -> n { prevNode = Just newNode })

remove :: Maybe (NodeRef a) -> IORef (Deque a) -> IO (Maybe a)
remove Nothing deque = return Nothing
remove (Just target) deque = do
    rNode <- readIORef target
    case prevNode rNode of
        Nothing -> modifyIORef deque (\d -> d { firstNode = nextNode rNode })
        Just pNode -> modifyIORef pNode (\n -> n { nextNode = nextNode rNode })
    case nextNode rNode of
        Nothing -> modifyIORef deque (\d -> d { lastNode = prevNode rNode })
        Just nNode -> modifyIORef nNode (\n -> n { prevNode = prevNode rNode })
    return $ Just (value rNode)