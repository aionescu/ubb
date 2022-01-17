module Main where

import Control.Category((>>>))
import Control.Monad((>=>))
import System.Environment(getArgs)

import Language.Xi.Eval(eval)
import Language.Xi.Parser(parse)
import Language.Xi.TypeChecker(typeCheck)

getCode :: IO String
getCode = do
  [path] <- getArgs
  case path of
    "-" -> getContents
    _ -> readFile path

main :: IO ()
main = getCode >>= ((parse >=> typeCheck) >>> either putStrLn eval)
