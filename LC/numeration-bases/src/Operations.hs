module Operations where

import Data.List
import Data.Maybe

type Digit = Char
type Digits = String
type Base = Int

digits :: String
digits = "0123456789ABCDEF"

(...) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(...) = (.) . (.)

indexOf :: Eq a => [a] -> a -> Int
indexOf = fromJust ... flip elemIndex

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