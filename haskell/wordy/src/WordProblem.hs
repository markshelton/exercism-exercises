module WordProblem (answer) where

import Control.Applicative ((<|>))
import Data.Either.Combinators (rightToMaybe)
import Parser (Parser(..), ParseResult, runParser, char, anyChar, chainl1, parens, reserved, number)

data Expr
    = Literal Integer
    | Add Expr Expr
    | Subtract Expr Expr
    | Multiply Expr Expr
    | Divide Expr Expr
    | Exponential Expr Expr
    deriving Show

int :: Parser Expr
int = do
  n <- number
  return $ Literal n

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

term :: Parser Expr
term = int <|> parens expr

opExponential :: Parser (Expr -> Expr -> Expr)
opExponential = infixOp "raised to" Exponential

opMultiply :: Parser (Expr -> Expr -> Expr)
opMultiply = infixOp "multiplied by" Multiply

opDivide :: Parser (Expr -> Expr -> Expr)
opDivide = infixOp "divided by" Divide

opAdd :: Parser (Expr -> Expr -> Expr)
opAdd = infixOp "plus" Add

opSubtract :: Parser (Expr -> Expr -> Expr)
opSubtract = infixOp "minus" Subtract

operation :: Parser (Expr -> Expr -> Expr)
operation = opMultiply <|> opDivide <|> opAdd <|> opSubtract <|> opExponential

expr :: Parser Expr
expr = term `chainl1` operation

exprTrimmed :: Parser Expr
exprTrimmed = let one = expr <|> (anyChar >> one) in one

exprFull :: Parser Expr
exprFull = do
  e <- exprTrimmed
  char '?'
  return e

run :: String -> ParseResult Expr
run = runParser exprFull

eval :: Expr -> Integer
eval (Literal a) = a 
eval (Add a b) = eval a + eval b
eval (Subtract a b) = eval a - eval b
eval (Multiply a b) = eval a * eval b
eval (Divide a b) = eval a `quot` eval b
eval (Exponential a b) = eval a ^ eval b

answer :: String -> Maybe Integer
answer = fmap eval . rightToMaybe . run