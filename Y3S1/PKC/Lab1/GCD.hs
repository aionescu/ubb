#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall -Wextra #-}
{-# LANGUAGE TypeApplications #-}

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
factors x = go [2 .. x `quot` 2] x
  where
    go [] _ = []
    go (f : fs) n
      | p > 0 = (f, p) : go fs n'
      | otherwise = go fs n
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
  print $ gcd [22, 10, product $ (^ (100 :: Integer)) <$> [2 .. 20]]
