-- This program generates a large Xi file that when interpreted creates
-- a large object in memory. Useful for stress-testing the interpreter.

-- You can run this file with `runhaskell GenLinear.hs`.

import Control.Monad(join)

genStressTest :: Int -> String
genStressTest n =
  "let a = { } in\n"
  ++ join (replicate n "let a = { a } in\n")
  ++ "print a" ++ join (replicate n ".a") ++ "\n"

main :: IO ()
main = writeFile "StressTests/Linear.xi" $ genStressTest 100000
