{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns, BlockArguments, TypeApplications #-}

import Control.Monad(when)
import Data.Foldable(for_)
import System.Environment(getArgs)
import System.Exit(exitSuccess)

sqrt' :: Integer -> Integer
sqrt' =  floor . sqrt @Double . fromIntegral

isPerfSquare :: Integer -> Bool
isPerfSquare n = n == s * s
  where
    s = sqrt' n

main :: IO ()
main = do
  [read -> b, read -> n] <- getArgs

  for_ [1 ..] \k -> do
    let t₀ = sqrt' (k * n)

    putStrLn $ "k = " <> show k
    putStrLn $ "t₀ = " <> show t₀

    for_ [1 .. b] \i -> do
      let t = t₀ + i
      let s² = t * t - k * n

      putStrLn $ "t = t₀ + " <> show i <> " ⟹  t² - " <> show k <> " * n = " <> show s²

      when (isPerfSquare s²) do
        let s = sqrt' s²

        putStrLn $ "\nn = 1/" <> show k <> " * " <> show (t - s) <> " * " <> show (t + s)
        exitSuccess

    putStrLn ""
