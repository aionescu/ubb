module Number where

import Data.List

type Digits = String
type Base = Int

data Number = Number Digits Base
  deriving Show

data Conversion = Conversion Number Base
  deriving Show

data Operator = Add | Sub | Mul | Div
  deriving (Show, Eq)

data Operation = Operation Number Operator Number
  deriving Show

data Cmd
  = CmdConv Conversion
  | CmdOp Operation
  deriving Show

digits :: String
digits = "0123456789ABCDEF"

toBase10 :: Char -> Int
toBase10 = (`elemIndex` digits)

baseToDigit :: Base -> String
baseToDigit 16 = "10"
baseToDigit n = digits !! n

doConversion :: Conversion -> Number
doConversion (Conversion n@(Number _ b) h)
  | b < h = substitutionMethod n h
  | b > h = successiveDivsMethod n h
  | otherwise = n

substitutionMethod :: Number -> Base -> Number
substitutionMethod (Number n b) h =
  let b' = baseToDigit b -- Converting b to b'(h)
  in Number n b

successiveDivsMethod :: Number -> Base -> Number
successiveDivsMethod n h = n

doOperation :: Operation -> Number
doOperation (Operation a op b)
  | op == Add = doAddition a b
  | op == Sub = doSubtraction a b
  | op == Mul = doMultiplication a b
  | otherwise = doDivision a b

doAddition :: Number -> Number -> Number
doAddition (Number a base) (Number b _) = undefined

doSubtraction :: Number -> Number -> Number
doSubtraction a b = a

doMultiplication :: Number -> Number -> Number
doMultiplication a b = a

doDivision :: Number -> Number -> Number
doDivision a b = a

processCmd :: Cmd -> Number
processCmd (CmdConv conv) = doConversion conv
processCmd (CmdOp op) = doOperation op