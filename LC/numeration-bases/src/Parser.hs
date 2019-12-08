module Parser where

import Data.Char(toUpper)
import Text.Parsec
import Text.Parsec.Char

import Number

type Parser = Parsec String ()

base :: Parser Int
base = between (char '(') (char ')') numWs
  where
    numWs = do
      spaces
      n <- read <$> many1 digit
      spaces

      if n >= 2 && n <= 16
      then pure n
      else fail "Base must be between 2 and 16."

number :: Parser Number
number = do
  n <- many1 (oneOf digits)
  spaces
  b <- base
  let ds = take b digits

  if any (not . (`elem` ds)) n
  then fail "Number must be in specified base."
  else pure $ Number n b

operator :: Parser Operator
operator = choice [add, sub, mul, div]
  where
    add = Add <$ char '+'
    sub = Sub <$ char '-'
    mul = Mul <$ char '*'
    div = Div <$ char '/'

conversion :: Parser Cmd
conversion = do
  n <- number
  spaces
  char '='
  spaces
  char '?'
  spaces
  b <- base
  pure $ CmdConv $ Conversion n b

operation :: Parser Cmd
operation = do
  a <- number
  spaces
  op <- operator
  spaces
  b <- number
  let (Number _ aBase) = a
  let (Number bNum bBase) = b

  if aBase /= bBase
  then fail "Both operands must be in the same base."
  else
    if (op == Mul || op == Div) && length bNum > 1
    then fail "Multiplication and division can only be performed on 1-digit numbers."
    else pure $ CmdOp $ Operation a op b

cmd :: Parser Cmd
cmd = do
  spaces
  c <- try conversion <|> operation
  spaces
  pure c

parse :: String -> Either String Cmd
parse input =
  let upper = toUpper <$> input in
  case runParser cmd () "" upper of
    Left err -> Left $ show err
    Right p -> Right p