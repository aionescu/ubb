{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Math where

import Data.Bifunctor(bimap)
import Data.Bits(shiftR, testBit)

(//) :: Integral a => a -> a -> a
(//) = div
infixl 7 //

(%) :: Integral a => a -> a -> a
(%) = mod
infixl 7 %

while :: (a -> Bool) -> (a -> a) -> a -> a
while p f x
  | p x = while p f (f x)
  | otherwise = x

-- Taken from https://rosettacode.org/wiki/Chinese_remainder_theorem#Haskell

egcd :: Integer -> Integer -> (Integer, Integer)
egcd _ 0 = (1, 0)
egcd a b = (t, s - q * t)
  where
    (s, t) = egcd b r
    (q, r) = a `divMod` b

modInv :: Integer -> Integer -> Integer
modInv a b
  | a * x + b * y == 1 = x
  | otherwise = error "modInv: No modular inverse"
  where
    (x, y) = egcd a b

chineseRemainder :: [Integer] -> [Integer] -> Integer
chineseRemainder residues modulii =
  (% modPI)
  $ sum
  $ zipWith (*) crtModulii
  $ zipWith (*) residues
  $ zipWith modInv crtModulii modulii
  where
    modPI = product modulii
    crtModulii = (modPI //) <$> modulii

-- Taken from https://gist.github.com/trevordixon/6788535

pow :: Integer -> Integer -> Integer -> Integer
pow _ 0 _ = 1
pow b e m
  | e < 0 = error "pow: Negative exponent"
  | otherwise = t * pow (b * b % m) (shiftR e 1) m % m
  where
    t = if testBit e 0 then b % m else 1

-- Translated from https://eli.thegreenplace.net/2009/03/07/computing-modular-square-roots-in-python

legendre :: Integer -> Integer -> Integer
legendre a p
  | ls == (p - 1) = -1
  | otherwise = ls
  where
    ls = pow a ((p - 1) // 2) p

modSqrt :: Integer -> Integer -> Integer
modSqrt a p
  | a == 0 || p == 2 || legendre a p /= 1 = error "modSqrt: No square root"
  | p % 4 == 3 = pow a ((p + 1) // 4) p
  | otherwise = go x b g r
  where
    (s, e) = while (even . fst) (bimap (// 2) (+ 1)) (p - 1, 0)
    n = while (\n -> legendre n p /= -1) (+ 1) 2

    x = pow a ((s + 1) // 2) p
    b = pow a s p
    g = pow n s p
    r = e

    go x b g r
      | m == 0 = x
      | otherwise =
          go
            (x * gs % p)
            (b * g' % p)
            g'
            m
      where
        (_, m) = while (\(t, m) -> m < r && t /= 1) (\(t, m) -> (t * t % p, m + 1)) (b, 0 :: Integer)
        gs = pow g (2 ^ (r - m - 1)) p
        g' = gs * gs % p

encryptRaw :: Integer -> Integer -> Integer
encryptRaw n m = m * m % n

decryptRaw :: Integer -> Integer -> Integer -> [Integer]
decryptRaw p q c =
  [ chineseRemainder [a, b] [p, q]
  , chineseRemainder [-a, b] [p, q]
  , chineseRemainder [a, -b] [p, q]
  , chineseRemainder [-a, -b] [p, q]
  ]
  where
    a = modSqrt c p
    b = modSqrt c q
