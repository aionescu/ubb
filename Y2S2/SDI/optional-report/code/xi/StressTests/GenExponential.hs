-- This program generates a very large Xi file that when interpreted creates
-- a very large object in memory. Useful for stress-testing the interpreter.

-- You can run this file with `runhaskell GenExponential.hs`.

import Control.Monad(join)
import Data.List(intercalate)

addA :: Int -> String
addA n = 'a' : show n

genLine :: Int -> String
genLine n = "let a" ++ show n ++ " = { " ++ intercalate ", " (addA <$> [0 .. n - 1]) ++ " } in\n"

genLets :: Int -> String
genLets 0 = "let a0 = { } in\n"
genLets n = genLets (n - 1) ++ genLine n

genExpr :: Int -> String
genExpr n = "print " ++ intercalate "." (addA <$> [n, n - 1 .. 0]) ++ "\n"

genStressTest :: Int -> String
genStressTest n = genLets n ++ genExpr n

main :: IO ()
main = writeFile "StressTests/Exponential.xi" $ genStressTest 1000
