module Parser where

import Data.Char (isDigit)
import Control.Monad (MonadPlus(..))
import Control.Applicative (Alternative(..))
    
newtype Parser a = Parser { parse :: String -> [(a, String)] }
type ParseResult a = Either String a

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure = return
  (<*>) (Parser cs1) (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  return a = Parser (\s -> [(a, s)])
  (>>=) p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

instance MonadPlus Parser where
  mzero = Parser (const [])
  mplus p q = Parser (\s -> parse p s ++ parse q s)

instance Alternative Parser where
  empty = mzero
  (<|>) p q = Parser $ \s -> 
    case parse p s of 
        [] -> parse q s
        res -> res

runParser :: Parser a  -> String -> ParseResult a
runParser m s = case parse m s of
  [(res, [])] -> Right res
  [(_, rs)] -> Left "Parser did not consume entire stream"
  _ -> Left "Parser error"

anyChar :: Parser Char
anyChar = Parser $ \s -> 
    case s of
      [] -> []
      (c:cs) -> [(c,cs)]
        
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = anyChar >>= \c -> if p c then return c else mzero

oneOf :: String -> Parser Char
oneOf = satisfy . flip elem

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do {a <- p; rest a}
  where rest a = (do f <- op; b <- p; rest (f a b)) <|> return a

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

char :: Char -> Parser Char
char = satisfy . (==)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string [] = return []
string (c:cs) = do 
  char c
  string cs
  return (c:cs)

token :: Parser a -> Parser a
token p = do 
  spaces
  a <- p 
  spaces
  return a

reserved :: String -> Parser String
reserved = token . string

spaces :: Parser String
spaces = many $ oneOf " \n\r"

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Integer
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n