module Main where

import Data.Function((&))

import Parser
import TypeCk
import Eval
import Opts

run :: Opts -> IO ()
run Run{..} = do
  code <- readFile path

  parse code
    >>= typeCheck
    >>= (if smallStep then (showSteps <$>) . allSteps else (showOut <$>) . eval)
    & either id id
    & putStrLn

run DumpAst{..} = do
  code <- readFile path

  parse code
    >>= (if noTypeCheck then pure else typeCheck)
    & either id show
    & putStrLn

main :: IO ()
main = getOpts >>= run
