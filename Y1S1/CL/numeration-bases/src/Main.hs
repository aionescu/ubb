module Main where

import Control.Monad(forever)
import Data.Char(toUpper)
import System.Exit(exitSuccess)
import System.IO(BufferMode(NoBuffering), hSetBuffering, stdout)

import Parser

helpText :: String
helpText = "\
  \numeration-bases (Made by Andrei-Alexandru Ionescu)\n\
  \To do a conversion, type 'n(b) = ?(h)'.\n\
  \To do an operation, type 'x(b) <op> y(b)'.\n\
  \To exit, type 'exit'.\n\
  \\n\
  \Here's some examples:\n\
  \2A(16) = ?(10)\n\
  \44(8) + 56(8)\n\
  \2B(12) / 2(12)\n"

handleCmd :: IO ()
handleCmd = do
  putStr "> "
  input <- (toUpper <$>) <$> getLine

  case input of
    "" -> pure ()
    "EXIT" -> exitSuccess
    _ -> putStrLn $ either id id $ parse input

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn helpText
  forever handleCmd
