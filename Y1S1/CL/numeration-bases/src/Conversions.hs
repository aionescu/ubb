module Conversions where

import Operations

-- Taken from https://stackoverflow.com/a/12882583
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (ys, zs) = splitAt n xs
  in ys : chunks n zs

-- This function converts a number using the substitution method.
-- Parameters:
--   src: The source base.
--   dest: The destination base.
--   num: The number to convert.
-- Preconditions: src < dest.
-- Returns: The result of the conversion.
substitution :: Base -> Base -> Digits -> Digits
substitution src dest num = loop 0 num
  where
    pow :: Base -> Digits -> Digit -> Digits
    pow 0 a b = a
    pow n a b = pow (n - 1) (mul dest a b) b

    b' = toDigit src
    
    loop i (a : as) = add dest (pow i [a] b') (loop (i + 1) as)
    loop i [] = "0"

-- This function converts a number using the method of successive divisions.
-- Parameters:
--   src: The source base.
--   dest: The destination base.
--   num: The number to convert.
-- Preconditions: src > dest.
-- Returns: The result of the conversion.
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

-- Powers of 2
pow2 :: [Base]
pow2 = [2, 4, 8, 16]

-- The `log` function in Haskell's standard library only works on Floats,
-- so we have to convert to Float and back.
log2 :: Int -> Int
log2 = floor . logBase 2.0 . fromIntegral

-- Correspondence table used for rapid conversions.
table :: [[Digits]]
table =
  [ []
  , reverse <$> ["0", "1", "10", "11", "100", "101", "110", "111", "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111"]
  , reverse <$> ["0", "1", "2", "3", "10", "11", "12", "13", "20", "21", "22", "23", "30", "31", "32", "33"]
  , reverse <$> ["0", "1", "2", "3", "4", "5", "6", "7", "10", "11", "12", "13", "14", "15", "16", "17"]
  , reverse <$> ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F"]
  ]

-- Helper functions
extendTo :: Int -> Digits -> Digits
extendTo n (a : as) = a : extendTo (n - 1) as
extendTo n [] = replicate n '0'

-- This function converts a single digit to base 2.
-- Preconditions: The source base is a power of 2.
to2 :: Base -> Digit -> Digits
to2 b d =
  let
    log = log2 b
    n = toBase10 d
    t = table !! 1
  in
    extendTo log (t !! n)

-- This function converts a number from base 2 to a single digit in another base.
-- Preconditions: The source base is a power of 2.
from2 :: Base -> Digits -> Digit
from2 b d =
  let
    log = log2 b
    t = table !! log
    i = indexOf (table !! 1) (skipTail0 d)
    [r] = t !! i
  in
    r

-- This function implements the rapid conversion from any base that is
-- a power of 2 to base 2.
rapidTo2 :: Base -> Digits -> Digits
rapidTo2 b = (>>= to2 b)

-- This function implements the rapid conversion from base 2 to
-- any base that is a power of 2.
rapidFrom2 :: Base -> Digits -> Digits
rapidFrom2 b d =
  let
    log = log2 b
    cs = extendTo log <$> chunks log d
  in
    from2 b <$> cs

-- Data type used to represent a substitution method.
data Method = Base10 | Rapid | Substitution | SuccessiveDivs | Identity

showMethod m =
  case m of
    Base10 -> "intermediary base 10"
    Rapid -> "rapid conversions"
    Substitution -> "substitution"
    SuccessiveDivs -> "successive divisions"
    Identity -> "identity"

-- This function chooses which substitution method to employ
-- based on the source and destination base.
convert' :: Base -> Base -> Digits -> (Digits, Method)
convert' src dest n
  | src == dest = (n, Identity)
  | src `elem` pow2 && dest `elem` pow2 =
      let b2 = rapidTo2 src n
      in (rapidFrom2 dest b2, Rapid)
  | src < 10 && dest < 10 =
      let (b10, _) = convert' src 10 n
      in (fst (convert' 10 dest b10), Base10)
  | src < dest = (substitution src dest n, Substitution)
  | src > dest = (successiveDivs src dest n, SuccessiveDivs)

-- This function pretty-prints the result of a conversion.
convert :: Base -> Base -> Digits -> String
convert src dest n =
  let (d, m) = convert' src dest n
  in showDigits dest d ++ " (via " ++ showMethod m ++ ")"