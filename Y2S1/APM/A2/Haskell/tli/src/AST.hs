module AST where

type Ident = String

data Type
  = TInt
  | TBool
  deriving stock Eq

instance Show Type where
  show TInt = "Int"
  show TBool = "Bool"

data Val
  = VBool Bool
  | VInt Int

instance Show Val where
  show (VBool b) = show b
  show (VInt i) = show i

data ArithOp
  = Add
  | Subtract
  | Multiply
  | Divide
  | Remainder
  deriving Eq

instance Show ArithOp where
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide = "/"
  show Remainder = "%"

arithOp :: Integral a => ArithOp -> a -> a -> a
arithOp Add = (+)
arithOp Subtract = (-)
arithOp Multiply = (*)
arithOp Divide = quot
arithOp Remainder = rem

data LogicOp
  = And
  | Or

instance Show LogicOp where
  show And = "and"
  show Or = "or"

logicOp :: LogicOp -> Bool -> Bool -> Bool
logicOp And = (&&)
logicOp Or = (||)

data CompOp
  = Gt
  | GtEq
  | Lt
  | LtEq
  | Eq
  | NEq

instance Show CompOp where
  show Gt = ">"
  show GtEq = ">="
  show Lt = "<"
  show LtEq = "<="
  show Eq = "="
  show NEq = "<>"

compOp :: Ord a => CompOp -> a -> a -> Bool
compOp Gt = (>)
compOp GtEq = (>=)
compOp Lt = (<)
compOp LtEq = (<=)
compOp Eq = (==)
compOp NEq = (/=)

data Expr
  = Lit Val
  | Var Ident
  | Arith Expr ArithOp Expr
  | Logic Expr LogicOp Expr
  | Comp Expr CompOp Expr

instance Show Expr where
  show (Lit v) = show v
  show (Var ident) = ident
  show (Arith a op b) = "(" ++ show a ++ " " ++ show op ++ " " ++ show b ++ ")"
  show (Logic a op b) = "(" ++ show a ++ " " ++ show op ++ " " ++ show b ++ ")"
  show (Comp a op b) = "(" ++ show a ++ " " ++ show op ++ " " ++ show b ++ ")"

data Stmt
  = Nop
  | Decl Ident Type
  | Assign Ident Expr
  | DeclAssign Ident Type Expr
  | Print Expr
  | If Expr Stmt Stmt
  | While Expr Stmt
  | Compound Stmt Stmt

instance Show Stmt where
  show Nop = ""
  show (Decl ident type') = ident ++ " : " ++ show type'
  show (Assign ident expr) = ident ++ " <- " ++ show expr
  show (DeclAssign ident type' expr) = ident ++ " : " ++ show type' ++ " <- " ++ show expr
  show (Print expr) = "print " ++ show expr
  show (If cond then' Nop) = "if " ++ show cond ++ " { " ++ show then' ++ " }"
  show (If cond then' else') =
    "if " ++ show cond ++ " { " ++ show then' ++ " } else {" ++ show else' ++ " }"
  show (While cond body) = "while " ++ show cond ++ " { " ++ show body ++ " }"
  show (Compound a b) = show a ++ "; " ++ show b

type Program = Stmt

type TLI a = Either String a

throw :: a -> Either a b
throw = Left

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right b) = Right b

toTLI :: Show a => Either a b -> TLI b
toTLI = mapLeft show
