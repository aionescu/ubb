{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

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

getS :: Integer -> Integer -> Integer -> IO Integer
getS n t0 i = do
  let t = t0 + i
  let s = t * t - n
  putStrLn $ "t = t₀ + " ++ show i ++ ": t² - n = " ++ show s

  if isPerfSquare s then
    pure s
  else
    getS n t0 (i + 1)

main :: IO ()
main = do
  [read -> n] <- getArgs

  when (isPerfSquare n) do
    putStrLn $ "ERROR: " ++ show n ++ " is a perfect square."
    exitFailure

  let t0 = sqrt' n

  putStrLn $ "t0 = " ++ show t0 ++ "\n"

  s2 <- getS n t0 1

  let t2 = s2 + n
  let t = sqrt' t2
  let s = sqrt' s2

  putStrLn $ "\ns = " ++ show s
  putStrLn $ "t = " ++ show t

  [a, b] <- pure $ sort [t - s, t + s]
  putStrLn $ "\nn = " ++ show a ++ " * " ++ show b

  when (a * b /= n) do
    putStrLn "\nERROR: Sanity check failed."
    exitFailure
