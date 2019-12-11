module Conversions where

import Operations

substitution :: Base -> Base -> Digits -> Digits
substitution src dest num = loop 0 num
  where
    pow :: Base -> Digits -> Digit -> Digits
    pow 0 a b = a
    pow n a b = pow (n - 1) (mul dest a b) b

    b' = toDigit src
    
    loop i (a : as) = add dest (pow i [a] b') (loop (i + 1) as)
    loop i [] = "0"

successiveDivs :: Base -> Base -> Digits -> Digits
successiveDivs src dest num = loop [] num
  where
    h = toDigit dest

    loop acc a =
      let (quot, rem) = divMod' src a h
      in
        if isZero quot
        then reverse (rem : acc)
        else loop (rem : acc) quot

convert' :: Base -> Base -> Digits -> Digits
convert' src dest n
  | src < 10 && dest < 10 =
      let b10 = convert' src 10 n
      in convert' 10 dest b10
  | src < dest = substitution src dest n
  | src > dest = successiveDivs src dest n
  | otherwise = n -- src == dest, no conversion needed

convert src dest n = showDigits dest $ convert' src dest n