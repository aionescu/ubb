module Parser where

import Data.Char(toUpper)
import Text.Parsec
import Text.Parsec.Char

import Operations
import Conversions

type Parser = Parsec String ()

base :: Parser Base
base = between (char '(') (char ')') numWs
  where
    numWs = do
      spaces
      n <- read <$> many1 digit
      spaces

      if n >= 2 && n <= 16
      then pure n
      else fail "Base must be between 2 and 16."

number :: Parser (Digits, Base)
number = do
  n <- many1 (oneOf digits)
  spaces
  b <- base
  let ds = take b digits

  if any (not . (`elem` ds)) n
  then fail "Number must be in specified base."
  else pure $ (reverse n, b)

operator :: Parser Op
operator = choice [add, sub, mul, div]
  where
    add = Add <$ char '+'
    sub = Sub <$ char '-'
    mul = Mul <$ char '*'
    div = DivMod <$ char '/'

conversion :: Parser String
conversion = do
  (n, b) <- number
  spaces
  char '='
  spaces
  char '?'
  spaces
  h <- base
  pure $ convert b h n

operation :: Parser String
operation = do
  (a, aBase) <- number
  spaces
  op <- operator
  spaces
  (b, bBase) <- number

  if aBase /= bBase
  then fail "Both operands must be in the same base."
  else
    if (op == Mul || op == DivMod) && length b > 1
    then fail "Multiplication and division can only be performed on 1-digit numbers."
    else pure $ eval op aBase a b

cmd :: Parser String
cmd = do
  spaces
  c <- try conversion <|> operation
  spaces
  pure c

parse :: String -> Either String String
parse input =
  let upper = toUpper <$> input in
  case runParser cmd () "" upper of
    Left err -> Left $ show err
    Right p -> Right p