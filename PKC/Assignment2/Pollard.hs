{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}

import Data.Char(intToDigit)
import Numeric(showIntAtBase)
import System.Environment (getArgs)
import Data.Foldable (traverse_)
import Control.Monad (when, unless)
import Data.List (isInfixOf, sort)
import System.Exit (exitSuccess, exitFailure)

sqrt' :: Integer -> Integer
sqrt' =  floor . sqrt @Double . fromIntegral

isPerfSquare :: Integer -> Bool
isPerfSquare n = n == s * s
  where
    s = sqrt' n

f :: Integer -> Integer -> Integer
f n x = (x * x + 1) `rem` n

seq' :: Integer -> [Integer]
seq' n = iterate (f n) 2

getD :: Integer -> [Integer] -> Int -> IO Integer
getD n xs j = do
  let
    a = xs !! j
    a' = xs !! (j * 2 - 1)
    b = xs !! (j * 2)
    d = gcd (abs $ b - a) n

  putStrLn $
    "x[" ++ show (j * 2 - 1) ++ "] = " ++ show a'
    ++ ", x[" ++ show (j * 2) ++ "] = " ++ show b
    ++ ", (x[" ++ show (j * 2) ++ "] - x[" ++ show j ++ "], n) = " ++ show d

  when (d == n) do
    putStrLn "FAILURE."
    exitSuccess

  if d == 1
  then getD n xs (j + 1)
  else pure d

main :: IO ()
main = do
  [read -> n] <- getArgs
  d <- getD n (seq' n) 1

  [a, b] <- pure $ sort [d, n `quot` d]
  putStrLn $ "n = " ++ show a ++ " * " ++ show b
