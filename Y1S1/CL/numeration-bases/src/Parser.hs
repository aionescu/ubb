module Parser(parse) where

import Control.Monad(join)
import Data.Bifunctor(first)
import Text.Parsec hiding (parse)

import Conversions
import Operations

-- This module implements the parsing algorithm used to interpret
-- expressions of the form `x(b) <op> y(b)` and `n(b) = ?(h)`.

type Parser = Parsec String ()

ch :: Char -> Parser Char
ch c = char c <* spaces

-- Parses a base enclosed in parentheses.
base :: Parser Base
base = between (ch '(') (ch ')') baseNum
  where
    baseNum = check . read =<< many1 digit <* spaces
    check n
      | n < 2 || n > 16 = fail "Base must be between 2 and 16."
      | otherwise = pure n

-- Parses a number, comprised of a series of digits and a base.
number :: Parser (Digits, Base)
number = join $ checkBase <$> hexNum <*> base
  where
    hexNum = many1 (oneOf hexDigits) <* spaces

    checkBase n b
      | all (`elem` baseDigits) n = pure (reverse n, b)
      | otherwise = fail "Number must be in specified base."
      where
        baseDigits = take b hexDigits

-- Parses a single arithmetic operator.
operator :: Parser Op
operator = oneOf "+-*/" <* spaces

-- Parses a conversion expression, then performs the conversion
-- and returns the result.
conversion :: Parser String
conversion = conv <$> (number <* ch '=' <* ch '?') <*> base
  where
    conv (n, b) h = convert b h n

-- Parses an operation expression, then performs the operation
-- and returns the result.
operation :: Parser String
operation = join $ checkOp <$> number <*> operator <*> number
  where
    checkOp (a, aBase) op (b, bBase)
      | aBase /= bBase = fail "Both operands must be in the same base."
      | op `elem` "*/" && length b > 1 = fail "Multiplication and division can only be performed on 1-digit numbers."
      | otherwise = pure $ eval op aBase a b

-- Parses a "command", i.e. either an operation or a conversion.
cmd :: Parser String
cmd = spaces *> (try conversion <|> operation) <* eof

parse :: String -> Either String String
parse = first show . runParser cmd () "<stdin>"
