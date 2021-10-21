#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall -Wextra -Wincomplete-patterns -Wincomplete-uni-patterns #-}

module GCD(main) where

import Data.List(nub, sort, transpose)
import Prelude hiding (gcd)

-- powerOf n f = (m, p) such that p ^ f * m = n
powerOf :: Integer -> Integer -> (Integer, Integer)
powerOf n f
  | n `rem` f == 0 = (+ 1) <$> powerOf (n `quot` f) f
  | otherwise = (n, 0)

-- factors n = [(f1, p1), ..., (fi, pi)] such that f1 ^ p1 * ... * fi ^ pi = n
factors :: Integer -> [(Integer, Integer)]
factors x = go [] [2 .. x `quot` 2] x
  where
    go [] [] _ = [(x, 1)]
    go acc [] _ = reverse acc
    go acc (f : fs) n
      | p > 0 = go ((f, p) : acc) fs n'
      | otherwise = go acc fs n
      where (n', p) = powerOf n f

-- Assuming sorted input
gcd' :: [Integer] -> Integer
gcd' [] = 1
gcd' [n] = n
gcd' (n : ns) = r
  where
    (fs, ps) = unzip $ factors n
    fs' = ps : ((\x -> snd . powerOf x <$> fs) <$> ns)
    r = product $ zipWith (^) fs $ minimum <$> transpose fs'

gcd :: [Integer] -> Integer
gcd = gcd' . nub . sort

main :: IO ()
main = do
  print $ gcd [10, 20, 30]
  print $ gcd [10, 15, 30]
  print $ gcd [22, 10, 30, 30]
  print $ gcd [22, 10, product [n ^ (100 :: Integer) | n <- [2 .. 20]]]
