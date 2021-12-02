{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns, BlockArguments, TypeApplications #-}

import Control.Monad(when)
import Data.List(sort)
import System.Exit(exitFailure)
import System.Environment(getArgs)

sqrt' :: Integer -> Integer
sqrt' =  floor . sqrt @Double . fromIntegral

isPerfSquare :: Integer -> Bool
isPerfSquare n = n == s * s
  where
    s = sqrt' n

getFactors :: Integer -> Integer -> Integer -> IO (Integer, Integer)
getFactors n t0 i = do
  let t = t0 + i
  let s² = t * t - n
  putStrLn $ "t = t₀ + " <> show i <> " ⟹  t² - n = " <> show s²

  if isPerfSquare s²
  then pure (t, sqrt' s²)
  else getFactors n t0 (i + 1)

main :: IO ()
main = do
  [read -> n] <- getArgs
  putStrLn $ "n = " <> show n

  when (isPerfSquare n) do
    putStrLn $ "Error: " <> show n <> " is a perfect square."
    exitFailure

  let t0 = sqrt' n
  putStrLn $ "t₀ = " <> show t0 <> "\n"

  (t, s) <- getFactors n t0 1

  putStrLn $ "\ns = " <> show s
  putStrLn $ "t = " <> show t

  let [a, b] = sort [t - s, t + s]
  putStrLn $ "\nn = " <> show a <> " * " <> show b

  when (a * b /= n) do
    putStrLn "\nError: Sanity check failed."
    exitFailure
