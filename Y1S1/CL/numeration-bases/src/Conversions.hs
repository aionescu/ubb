module Conversions(convert) where

import Data.List.Extra(chunksOf)

import Operations

-- This function converts a number using the substitution method.
-- Preconditions: src < dest.
substitution :: Base -> Base -> Digits -> Digits
substitution src dest = go 0
  where
    b' = toDigit src

    pow :: Base -> Digits -> Digit -> Digits
    pow 0 a _ = a
    pow n a b = pow (n - 1) (mul dest a b) b

    go i (a : as) = add dest (pow i [a] b') (go (i + 1) as)
    go _ [] = "0"

-- This function converts a number using the method of successive divisions.
-- Preconditions: src > dest.
successiveDivs :: Base -> Base -> Digits -> Digits
successiveDivs src dest = go []
  where
    h = toDigit dest

    go acc a
      | isZero q = reverse $ r : acc
      | otherwise = go (r : acc) q
      where
        (q, r) = divMod' src a h

pow2 :: [Base]
pow2 = [2, 4, 8, 16]

log2 :: Int -> Int
log2 = floor @Double . logBase 2.0 . fromIntegral

-- Correspondence table used for rapid conversions.
table :: [[Digits]]
table =
  (reverse <$>) <$>
  [ []
  , ["0", "1", "10", "11", "100", "101", "110", "111", "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111"]
  , ["0", "1", "2", "3", "10", "11", "12", "13", "20", "21", "22", "23", "30", "31", "32", "33"]
  , ["0", "1", "2", "3", "4", "5", "6", "7", "10", "11", "12", "13", "14", "15", "16", "17"]
  , ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F"]
  ]

extendTo :: Int -> Digits -> Digits
extendTo n l = take n $ l <> repeat '0'

-- This function converts a single digit to base 2.
-- Preconditions: The source base is a power of 2.
to2 :: Base -> Digit -> Digits
to2 b d = extendTo (log2 b) $ table !! 1 !! toBase10 d

-- This function converts a number from base 2 to a single digit in another base.
-- Preconditions: The source base is a power of 2.
from2 :: Base -> Digits -> Digit
from2 b d = head $ t !! i
  where
    t = table !! log2 b
    i = indexOf (table !! 1) $ dropTail0 d

-- This function implements the rapid conversion from any base that is
-- a power of 2 to base 2.
rapidTo2 :: Base -> Digits -> Digits
rapidTo2 = foldMap . to2

-- This function implements the rapid conversion from base 2 to
-- any base that is a power of 2.
rapidFrom2 :: Base -> Digits -> Digits
rapidFrom2 b d = from2 b . extendTo l <$> chunksOf l d
  where
    l = log2 b

type Method = String

-- This function chooses which substitution method to employ
-- based on the source and destination base.
convert' :: Base -> Base -> Digits -> (Digits, Method)
convert' src dest n
  | src == dest = (n, "identity")
  | src `elem` pow2 && dest `elem` pow2 = (rapidFrom2 dest $ rapidTo2 src n, "rapid conversions")
  | src < 10 && dest < 10 = (conv 10 dest $ conv src 10 n, "intermediary base 10")
  | src < dest = (substitution src dest n, "substitution")
  | otherwise = (successiveDivs src dest n, "successive divisions")
  where
    conv s d = fst . convert' s d

-- This function pretty-prints the result of a conversion.
convert :: Base -> Base -> Digits -> String
convert src dest n = showDigits dest d <> " (via " <> m <> ")"
  where
    (d, m) = convert' src dest n
