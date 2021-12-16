{-# OPTIONS_GHC -Wall -Wno-type-defaults #-}
{-# LANGUAGE ViewPatterns, BlockArguments, TypeApplications #-}

import Control.Monad(when)
import Data.Foldable(for_)
import System.Exit(exitSuccess)
import Text.Printf(printf)

(//) :: Integer -> Integer -> Integer
(//) = quot
infixl 7 //

sqrt' :: Integer -> Integer
sqrt' = floor . sqrt @Double . fromIntegral

isPerfSquare :: Integer -> Bool
isPerfSquare n = n == s * s
  where
    s = sqrt' n

main :: IO ()
main = do
  let n = 2 ^ 51 - 1
  let b = 100

  for_ [1 ..] \k -> do
    let t₀ = sqrt' (k * n)

    printf "k = %i\n" k
    printf "t₀ = %i\n" t₀

    for_ [1 .. b] \i -> do
      let t = t₀ + i
      let s² = t * t - k * n

      printf "t = t₀ + %i ⟹ t² - %i * n = %i\n" i k s²

      when (isPerfSquare s²) do
        let s = sqrt' s²

        let (fk₁, fk₂) = (t - s, t + s)
        printf "\nn = 1/%i * %i * %i\n" k fk₁ fk₂

        let e = gcd k fk₁
        let e2 = k // e

        let (f₁, f₂) = (fk₁ // e, fk₂ // e2)
        printf "n = %i * %i\n" f₁ f₂

        exitSuccess

    putStrLn ""
