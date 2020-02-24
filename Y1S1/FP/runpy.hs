#!/usr/bin/env stack
-- stack --resolver lts-14.18 script

{-# LANGUAGE LambdaCase #-}

import System.Directory
import System.Environment
import System.Exit
import System.Process

myPy :: String -> String -> IO ExitCode
myPy strict path = system ("python3 -m mypy " ++ strict ++ " " ++ path)

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

getPath :: String -> String -> IO String
getPath pwd path = do
  let rel = pwd ++ "/" ++ path
  exists <- doesFileExist rel

  pure $
    if exists
    then rel
    else path

handleArgs :: IO (String, String)
handleArgs = do
  args <- getArgs
  pwd <- getCurrentDirectory

  case args of
    "--no-strict" : arg : _ -> do
      path <- getPath pwd arg
      pure ("", path)
    arg : _ -> do
      path <- getPath pwd arg
      pure ("--strict", arg)

main :: IO ()
main = do
  (strict, path) <- handleArgs

  c <-
    myPy strict path
    !=> tests path
    !=> run path

  exitWith c