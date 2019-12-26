#!/usr/bin/env stack
-- stack --resolver lts-14.18 script

{-# LANGUAGE LambdaCase #-}

import System.Directory
import System.Environment
import System.Exit
import System.Process

myPy :: String -> IO ExitCode
myPy path = system ("python3 -m mypy " ++ path)

tests :: String -> IO ExitCode
tests path = do
  let test = path ++ "/test.py"
  exists <- doesFileExist test
  
  if exists
  then system ("python3 " ++ test)
  else pure ExitSuccess

run :: String -> IO ExitCode
run path = system ("python3 " ++ path ++ "/main.py")

(!=>) :: IO ExitCode -> IO ExitCode -> IO ExitCode
(!=>) a b = do
  c <- a

  case c of
    ExitFailure _ -> pure c
    ExitSuccess -> b

getPath :: IO String
getPath = do
  args <- getArgs
  dir <- getCurrentDirectory

  case args of
    [] -> pure dir
    p : _ ->
      let rel = dir ++ "/" ++ p
      in do
        exists <- doesFileExist rel

        pure $
          if exists
          then rel
          else p

main :: IO ()
main = do
  path <- getPath

  c <-
    myPy path
    !=> tests path
    !=> run path

  exitWith c