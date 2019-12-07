module Main where

import Control.Monad(forever)
import Data.Maybe(fromMaybe)
import System.Exit(exitSuccess)
import System.IO(hSetBuffering, stdout, BufferMode(NoBuffering))

import Number
import Parser

helpText :: String
helpText = "\
  \numeration-bases (Made by Alex Ionescu)\n\
  \To do a conversion, type 'n(b) = ?(h)'.\n\
  \To do an operation, type 'n1(b) <op> n2(b)'.\n\
  \\n\
  \Here's some examples:\n\
  \2A(16) = ?(10)\n\
  \44(8) + 56(8)\n\
  \2B(12) / 2(12)\n"

handleCmd :: IO ()
handleCmd = do
  putStr "> "
  str <- getLine
  let res = parse str
  let output = (case res of Left err -> err; Right cmd -> processCmd cmd)

  putStrLn output

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn helpText
  forever handleCmd