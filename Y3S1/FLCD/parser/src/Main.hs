module Main(main) where

import Control.Monad(when)
import Data.Foldable(for_)
import System.Environment(getArgs)
import System.Exit(exitFailure)

import Grammar.Core
import Grammar.Parser
import Parsing.RecursiveDescent
import Parsing.Output
import Scanner.ST
import Scanner.Core
import FA.Parser

getPIF :: FilePath -> FilePath -> IO (PIF, IT, CT)
getPIF tokensPath inputPath = do
  Right idFA <- parseFA <$> readFile "Data/FA/Id.in"
  Right constFA <- parseFA <$> readFile "Data/FA/Const.in"
  tokens <- lines <$> readFile tokensPath
  input <- readFile inputPath

  runScanner tokens idFA constFA input >>= \case
    Left err -> print err *> exitFailure
    Right r -> pure r

main :: IO ()
main = do
  tokensPath : cfgPath : inputPath : debugArg <- getArgs

  let debug = debugArg == ["--debug"]
  (pif, _, _) <- getPIF tokensPath inputPath
  cfgInput <- readFile cfgPath

  case parse (full grammar) cfgInput of
    Left err -> putStrLn $ "Parser error:\n" ++ show err
    Right (checkCFG -> Just g)
      | hasLeftRecursion g -> putStrLn "Error: Left recursion detected"
      | otherwise -> do
          let states = recursiveDescent g pif

          when debug do
            for_ states \case
              (s, "") -> print s
              (s, mv) -> putStrLn $ show s <> " :: " <> mv

            putStrLn ""

          case fst $ last states of
            (F, _, rs, _) -> do
              putStrLn $ showTree $ genTree g rs
              putStrLn ""
              putStrLn $ showTable $ genTable g rs
            _ -> putStrLn "Parsing failed"
    _ -> putStrLn "Grammar must be context-free"
