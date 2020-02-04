module Operations where

import Data.List
import Data.Maybe

-- Data type declarations
type Digit = Char
type Digits = String
type Base = Int

-- Helper functions
digits :: String
digits = "0123456789ABCDEF"

indexOf :: Eq a => [a] -> a -> Int
indexOf xs x = fromJust $ elemIndex x xs

toBase10 :: Digit -> Int
toBase10 = indexOf digits

toDigit :: Int -> Digit
toDigit = (digits !!)

isZero :: Digits -> Bool
isZero = all (== '0')

skip0 :: Digits -> Digits
skip0 [] = []
skip0 "0" = "0"
skip0 ('0' : ds) = skip0 ds
skip0 ds = ds

skipTail0 :: Digits -> Digits
skipTail0 = reverse . skip0 . reverse

showDigits :: Base -> Digits -> String
showDigits base digits =
  let digits' = skip0 $ reverse digits
  in digits' ++ "(" ++ show base ++ ")"

-- This function performs the addition of 2 numbers in the specified base.
-- Parameters:
--   base: The base in which to perform the operation.
--   x: The first operand.
--   y: The second operand.
-- Returns: The result of adding the 2 operands.
add :: Base -> Digits -> Digits -> Digits
add base x y = loop 0 x y
  where
    step carry a b =
      let i = carry + toBase10 a + toBase10 b
      in i `quotRem` base

    loop carry (a : as) (b : bs) =
      let (quot, rem) = step carry a b
      in toDigit rem : (loop quot as bs)

    loop carry [] (b : bs) =
      let (quot, rem) = step carry '0' b
      in toDigit rem : (loop quot [] bs)

    loop carry (a : as) [] =
      let (quot, rem) = step carry a '0'
      in toDigit rem : (loop quot as [])

    loop 0 [] [] = []
    loop carry [] [] = [toDigit carry]

-- This function performs the subtraction of 2 numbers in the specified base.
-- Parameters:
--   base: The base in which to perform the operation.
--   x: The first operand.
--   y: The second operand.
-- Returns: The result of subtracting the second operand from the first.
sub :: Base -> Digits -> Digits -> Digits
sub base x y = loop x y
  where
    prevDigit = toDigit . subtract 1 . toBase10
    borrow (a : as) =
      if a == '0'
      then toDigit (base - 1) : borrow as
      else prevDigit a : as

    loop (a : as) (b : bs) =
      if a < b
      then
        let d = toDigit (toBase10 a + base - toBase10 b)
        in d : loop (borrow as) bs
      else
        let d = toDigit (toBase10 a - toBase10 b)
        in d : loop as bs

    loop [] (b : bs) = error "Cannot subtract a larger number from a smaller one."
    loop as@(_ : _) [] = as

    loop [] [] = []

-- This function performs the multiplication of 2 numbers in the specified base.
-- Parameters:
--   base: The base in which to perform the operation.
--   x: The first operand.
--   y: The second operand. Must be a single digit.
-- Returns: The result of multiplying the 2 operands.
mul :: Base -> Digits -> Digit -> Digits
mul base x y = loop 0 x
  where
    b = y
    step carry a b =
      let i = carry + toBase10 a * toBase10 b
      in i `quotRem` base

    loop carry (a : as) =
      let (quot, rem) = step carry a b
      in toDigit rem : (loop quot as)

    loop 0 [] = []
    loop carry [] = [toDigit carry]

-- This function performs the division of 2 numbers in the specified base.
-- Parameters:
--   base: The base in which to perform the operation.
--   x: The first operand.
--   y: The second operand. Must be a single digit.
-- Returns: A tuple containing the quotient and the remainder of the division of the 2 operands.
divMod' :: Base -> Digits -> Digit -> (Digits, Digit)
divMod' base x y = loop [] 0 (reverse x)
  where
    b = toBase10 y
    loop acc carry (a : as) =
      let
        a' = toBase10 a + base * carry
        (i, carry') = a' `quotRem` b
      in
        loop (toDigit i : acc) carry' as

    loop acc carry [] = (acc, toDigit carry)

data Op = Add | Sub | Mul | DivMod deriving Eq

eval :: Op -> Base -> Digits -> Digits -> String
eval Add b x y = showDigits b $ add b x y
eval Sub b x y = showDigits b $ sub b x y
eval Mul b x [y] = showDigits b $ mul b x y
eval DivMod b x [y] =
  let (quot, rem) = divMod' b x y
  in "q = " ++ showDigits b quot ++ ", r = " ++ showDigits b [rem]