module Opts where

import Options.Generic

data Opts
  = GenKey FilePath
  | ShowKey FilePath
  | Encrypt FilePath FilePath
  | Decrypt FilePath FilePath
  deriving stock Generic

instance ParseRecord Opts where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

getOpts :: IO Opts
getOpts = getRecord "Rabin Cryptosystem"
