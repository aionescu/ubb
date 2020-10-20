module Opts(Opts(..), getOpts) where

import Options.Generic

data Opts
  = Run { smallStep :: Bool, path :: String }
  | DumpAst { noTypeCheck :: Bool, path :: String }
  deriving stock Generic

instance ParseRecord Opts where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

getOpts :: IO Opts
getOpts = getRecord "Toy Language Interpreter"
