#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall -Wextra -Wincomplete-patterns -Wincomplete-uni-patterns #-}

module Main(main) where

import Data.List(nub, sort, transpose, foldl1')
import Test.QuickCheck

-- powerOf n f = (m, p) such that p ^ f * m = n
powerOf :: Int -> Int -> (Int, Int)
powerOf n f
  | n `rem` f == 0 = (+ 1) <$> powerOf (n `quot` f) f
  | otherwise = (n, 0)

-- factors n = [(f1, p1), ..., (fi, pi)] such that f1 ^ p1 * ... * fi ^ pi = n
factors :: Int -> [(Int, Int)]
factors x = go [] [2 .. x `quot` 2] x
  where
    go [] [] _ = [(x, 1)]
    go acc [] _ = reverse acc
    go acc (f : fs) n
      | p > 0 = go ((f, p) : acc) fs n'
      | otherwise = go acc fs n
      where (n', p) = powerOf n f

gcd' :: [Int] -> Int
gcd' = go . nub . sort
  where
    go [] = 1
    go [n] = n
    go (n : ns) = r
      where
        (fs, ps) = unzip $ factors n
        fs' = ps : ((\x -> snd . powerOf x <$> fs) <$> ns)
        r = product $ zipWith (^) fs $ minimum <$> transpose fs'

preludeGCD :: [Int] -> Int
preludeGCD [] = 1
preludeGCD ns = foldl1' gcd ns

propGCD :: [Int] -> Bool
propGCD ns = gcd' ns == preludeGCD ns

main :: IO ()
main = do
  quickCheck propGCD
