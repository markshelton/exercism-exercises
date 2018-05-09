module BankAccount (BankAccount, closeAccount, getBalance, incrementBalance, openAccount) where

import Data.IORef (IORef, newIORef, modifyIORef, readIORef)

data BankAccount = BankAccount { balance :: Integer, opened :: Bool } deriving (Eq, Show)

openAccount :: IO (IORef BankAccount)
openAccount = newIORef BankAccount {balance = 0, opened = True}

closeAccount :: IORef BankAccount -> IO ()
closeAccount account = modifyIORef account (\a -> a {opened = False})

getBalance :: IORef BankAccount -> IO (Maybe Integer)
getBalance account = do
    account <- readIORef account
    return $ if opened account 
        then Just (balance account)
        else Nothing

incrementBalance :: IORef BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance account amount = do
    accountRead <- readIORef account
    if opened accountRead 
        then (do
            let newBalance = balance accountRead + amount
            modifyIORef account (\a -> a {balance = newBalance})
            return $ Just newBalance)
        else return Nothing

