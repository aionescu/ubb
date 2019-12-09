module Main where

import Control.Monad(forever)
import Data.Maybe(fromMaybe)
import System.Exit(exitSuccess)
import System.IO(hSetBuffering, stdout, BufferMode(NoBuffering))

import Parser

helpText :: String
helpText = "\
  \numeration-bases (Made by Andrei-Alexandru Ionescu)\n\
  \To do a conversion, type 'n(b) = ?(h)'.\n\
  \To do an operation, type 'n1(b) <op> n2(b)'.\n\
  \To exit, type 'exit'.\n\
  \\n\
  \Here's some examples:\n\
  \2A(16) = ?(10)\n\
  \44(8) + 56(8)\n\
  \2B(12) / 2(12)\n"

handleCmd :: IO ()
handleCmd = do
  putStr "> "
  str <- getLine

  if str == "exit"
  then exitSuccess
  else
    let
      res = parse str
      output =
        case res of
          Left err -> err
          Right cmd -> cmd
    in
      putStrLn output

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn helpText
  forever handleCmd