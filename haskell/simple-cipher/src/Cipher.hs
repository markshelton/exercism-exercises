module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (isLower, ord, chr)
import Data.Maybe (mapMaybe)
import Control.Monad (replicateM)
import System.Random (getStdRandom, randomR)

caesarDecode :: String -> String -> String
caesarDecode key = zipWith decodeChar (parseKey (cycle key))

caesarEncode :: String -> String -> String
caesarEncode key = zipWith encodeChar (parseKey (cycle key))

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
    let keyLength = 100
    key <- generateKey keyLength
    return (key, caesarEncode key text)

encodeChar :: Int -> Char -> Char
encodeChar n x = chr $ (ord x + n - ord 'a') `mod` 26 + ord 'a'

decodeChar :: Int -> Char -> Char
decodeChar n = encodeChar (negate n)

generateKey :: Int -> IO String
generateKey n = replicateM n generateKeyCode

generateKeyCode :: IO Char
generateKeyCode = getStdRandom $ randomR ('a','z')

parseKey :: String -> [Int]
parseKey = mapMaybe parseKeyCode

parseKeyCode :: Char -> Maybe Int
parseKeyCode x
    | isLower x = Just $ ord x - ord 'a'
    | otherwise = Nothing