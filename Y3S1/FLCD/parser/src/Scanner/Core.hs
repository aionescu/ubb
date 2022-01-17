module Scanner.Core(Token(..), PIF, LexicalError, runScanner) where

import Control.Monad(when)
import Control.Monad.Except(throwError, ExceptT, runExceptT)
import Control.Monad.Extra(unlessM)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Reader(ReaderT(runReaderT), ask)
import Control.Monad.State(StateT, put, get, execStateT, gets)
import Data.Char(isSpace)
import Data.Function((&))
import Data.List(isPrefixOf)
import Data.List.Extra(maximumOn)
import System.Exit(exitFailure)

import FA.Core
import Scanner.ST

data Token
  = Token String
  | Id Int
  | Const Int
  deriving stock Show

type PIF = [Token]

data Config =
  Config
  { configTokens :: [String]
  , configIdFA :: FA
  , configConstFA :: FA
  }
  deriving stock Show

data ScanState =
  SS
  { ssIT :: IT
  , ssCT :: CT
  , ssPIF :: PIF
  , ssInput :: String
  , ssLine :: Int
  , ssCol :: Int
  }
  deriving stock Show

type Pos = (Int, Int)

data LexicalError
  = InvalidIdentifier String Pos
  | UnknownToken Pos

showPos :: Pos -> String
showPos (ssLine, ssCol) = "Ln " <> show ssLine <> ", Col " <> show ssCol

instance Show LexicalError where
  show e = "Lexical Error: " <> case e of
    InvalidIdentifier id' pos -> "Invalid identifier " <> show id' <> " at " <> showPos pos
    UnknownToken pos -> "Unkown token at " <> showPos pos

type Scanner = ReaderT Config (StateT ScanState (ExceptT LexicalError IO))

skipWS :: Scanner ()
skipWS = do
  SS{..} <- get
  case ssInput of
    '\n' : cs -> put SS{ssLine = ssLine + 1, ssCol = 1, ssInput = cs, ..} *> skipWS
    c : cs | isSpace c -> put SS{ssCol = ssCol + 1, ssInput = cs, ..} *> skipWS
    _ -> pure ()

detect :: Scanner ()
detect = do
  Config{..} <- ask
  SS{..} <- get

  let checkId = checkPrefix configIdFA
  let checkConst = checkPrefix configConstFA

  if
    | Just (id', rest) <- checkId ssInput  ->
        if id' `elem` configTokens then
          put SS{ssPIF = Token id' : ssPIF, ssInput = rest, ssCol = ssCol + length id', ..}
        else
          let (ix, newIT) = findOrInsert id' ssIT
          in put SS{ssIT = newIT, ssPIF = Id ix : ssPIF, ssInput = rest, ssCol = ssCol + length id', ..}

    | Just (const', rest) <- checkConst ssInput
    , Just (id', _) <- checkId rest ->
        throwError $ InvalidIdentifier (const' <> id') (ssLine, ssCol)

    | Just (const', rest) <- checkConst ssInput ->
        let (ix, newCT) = findOrInsert (read const') ssCT
        in put SS{ssCT = newCT, ssPIF = Const ix : ssPIF, ssInput = rest, ssCol = ssCol + length const', ..}

    | otherwise ->
        case filter (`isPrefixOf` ssInput) configTokens of
          [] -> throwError $ UnknownToken (ssLine, ssCol)
          (maximumOn length -> t) -> put SS{ssPIF = Token t : ssPIF, ssInput = drop (length t) ssInput, ssCol = ssCol + length t, ..}

detect1 :: Scanner ()
detect1 = do
  s <- gets ssInput
  detect
  s' <- gets ssInput

  liftIO $ when (s == s') do
    putStrLn "Loop in `detect`"
    putStrLn $ "ssInput was: " <> s
    exitFailure

scan :: Scanner ()
scan = skipWS *> unlessM (gets $ null . ssInput) (detect1 *> scan)

runScanner :: [String] -> FA -> FA -> String -> IO (Either LexicalError (PIF, IT, CT))
runScanner tokens idFA constFA input =
  scan
  & flip runReaderT (Config tokens idFA constFA)
  & flip execStateT (SS emptyST emptyST [] input 1 1)
  & fmap (\SS{..} -> (reverse ssPIF, ssIT, ssCT))
  & runExceptT
