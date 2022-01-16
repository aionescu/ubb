{-# OPTIONS_GHC -Wall -Wno-type-defaults #-}
{-# LANGUAGE BlockArguments, TypeApplications #-}

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
    let t0 = sqrt' (k * n)

    printf "k = %i\n" k
    printf "t0 = %i\n" t0

    for_ [1 .. b] \i -> do
      let t = t0 + i
      let s2 = t * t - k * n

      printf "t = t0 + %i => t ^ 2 - %i * n = %i\n" i k s2

      when (isPerfSquare s2) do
        let s = sqrt' s2

        let (fk1, fk2) = (t - s, t + s)
        printf "\nn = 1/%i * %i * %i\n" k fk1 fk2

        let e = gcd k fk1
        let e2 = k // e

        let (f1, f2) = (fk1 // e, fk2 // e2)
        printf "n = %i * %i\n" f1 f2

        exitSuccess

    putStrLn ""
