{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}

import Data.Char(intToDigit)
import Numeric(showIntAtBase)
import System.Environment (getArgs)
import Data.Foldable (traverse_)
import Control.Monad (when, unless)
import Data.List (isInfixOf)
import System.Exit (exitSuccess)

splitST :: Integer -> (Integer, Integer)
splitST n = go 0 (n - 1)
  where
    go acc n
      | r == 0 = go (acc + 1) q
      | otherwise = (acc, n)
      where
        (q, r) = n `quotRem` 2

showBin :: Integer -> String
showBin n = showIntAtBase 2 intToDigit n ""

print2 :: (Int, Integer) -> IO ()
print2 (i, n) = putStrLn $ "2^(2^" <> show i <> ") = " <> show n

printT :: Integer -> Integer -> (Int, Integer) -> IO ()
printT b t (i, n) = putStrLn $ show b <> "^(2^" <> show i <> " * " <> show t <> ") = " <> show n

isGood :: Integer -> [Integer] -> Bool
isGood n a = head a == 1 || isInfixOf [1, 1] a || isInfixOf [n - 1, 1] a

main :: IO ()
main = do
  [read -> n] <- getArgs

  let (s, t) = splitST n
  let tBin = showBin t
  let bitCount = length tBin

  putStrLn $ "n = " <> show n
  putStrLn $ "s = " <> show s
  putStrLn $ "t = " <> show t
  putStrLn ""

  putStrLn $ "tBin = " <> tBin
  putStrLn $ "bitCount = " <> show bitCount
  putStrLn ""

  traverse_ print2 [(i, 2 ^ (2 ^ i) `mod` n) | i <- [0 .. bitCount - 1]]
  putStrLn ""

  let a2 = [2 ^ ((2 ^ i) * t)  `mod` n | i <- [0 .. s]]
  traverse_ (printT 2 t) $ zip [0..] a2
  putStrLn ""

  unless (isGood n a2) do
    putStrLn "No"
    exitSuccess

  let a3 = [3 ^ ((2 ^ i) * t)  `mod` n | i <- [0 .. s]]
  traverse_ (printT 3 t) $ zip [0..] a3
  putStrLn ""

  unless (isGood n a3) do
    putStrLn "No"
    exitSuccess

  let a5 = [5 ^ ((2 ^ i) * t)  `mod` n | i <- [0 .. s]]
  traverse_ (printT 5 t) $ zip [0..] a5
  putStrLn ""

  unless (isGood n a5) do
    putStrLn "No"
    exitSuccess

  putStrLn "Yes"
