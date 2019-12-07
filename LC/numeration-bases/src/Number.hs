module Number where

data Number = Number String Int
  deriving Show

data Conversion = Conversion Number Int
  deriving Show

data Operator = Add | Sub | Mul | Div
  deriving Show

data Operation = Operation Number Operator Number
  deriving Show

data Cmd
  = CmdConv Conversion
  | CmdOp Operation
  deriving Show

digits :: String
digits = "0123456789ABCDEF"

processCmd :: Cmd -> String
processCmd = undefined