module Operations where

import Data.List
import Data.Maybe

-- Data type declarations
type Digit = Char
type Digits = String
type Base = Int

-- Helper functions
hexDigits :: String
hexDigits = "0123456789ABCDEF"

indexOf :: Eq a => [a] -> a -> Int
indexOf xs x = fromJust $ elemIndex x xs

toBase10 :: Digit -> Int
toBase10 = indexOf hexDigits

toDigit :: Int -> Digit
toDigit = (hexDigits !!)

isZero :: Digits -> Bool
isZero = all (== '0')

drop0 :: Digits -> Digits
drop0 l =
  case dropWhile (== '0') l of
    [] -> "0"
    xs -> xs

dropTail0 :: Digits -> Digits
dropTail0 = reverse . drop0 . reverse

showDigits :: Base -> Digits -> String
showDigits base digits = digits' <> "(" <> show base <> ")"
  where
    digits' = drop0 $ reverse digits

-- This function performs the addition of 2 numbers in the specified base.
add :: Base -> Digits -> Digits -> Digits
add base = go 0
  where
    step c a b = (c + toBase10 a + toBase10 b) `quotRem` base

    go 0 [] [] = []
    go c [] [] = [toDigit c]
    go c (a : as) (b : bs) = toDigit r : go q as bs
      where
        (q, r) = step c a b
    go c [] (b : bs) = toDigit r : go q [] bs
      where
        (q, r) = step c '0' b
    go c (a : as) [] = toDigit r : go q as []
      where
        (q, r) = step c a '0'

-- This function performs the subtraction of 2 numbers in the specified base.
sub :: Base -> Digits -> Digits -> Digits
sub base = go
  where
    prevDigit = toDigit . subtract 1 . toBase10

    borrow [] = error "Cannot borrow from 0"
    borrow ('0' : as) = toDigit (base - 1) : borrow as
    borrow (a : as) = prevDigit a : as

    go [] [] = []
    go as@(_ : _) [] = as
    go [] (_ : _) = error "Cannot subtract a larger number from a smaller one."
    go (a : as) (b : bs)
      | a < b = toDigit (base + toBase10 a - toBase10 b) : go (borrow as) bs
      | otherwise = toDigit (toBase10 a - toBase10 b) : go as bs

-- This function performs the multiplication of 2 numbers in the specified base.
mul :: Base -> Digits -> Digit -> Digits
mul base x y = go 0 x
  where
    step c a b = (c + toBase10 a * toBase10 b) `quotRem` base

    go 0 [] = []
    go c [] = [toDigit c]
    go c (a : as) = toDigit r : go q as
      where
        (q, r) = step c a y

-- This function performs the division of 2 numbers in the specified base,
-- computing both the quotient and the remainder.
divMod' :: Base -> Digits -> Digit -> (Digits, Digit)
divMod' base x y = go [] 0 $ reverse x
  where
    b = toBase10 y

    go acc c [] = (acc, toDigit c)
    go acc c (a : as) = go (toDigit q : acc) r as
      where
        (q, r) = (toBase10 a + base * c) `quotRem` b

type Op = Char

eval :: Op -> Base -> Digits -> Digits -> String
eval '+' b x y = showDigits b $ add b x y
eval '-' b x y = showDigits b $ sub b x y
eval '*' b x [y] = showDigits b $ mul b x y
eval '/' b x [y] = "q = " <> showDigits b q <> ", r = " <> showDigits b [r]
  where
    (q, r) = divMod' b x y
eval _ _ _ _ = error "Unsupported operation"
