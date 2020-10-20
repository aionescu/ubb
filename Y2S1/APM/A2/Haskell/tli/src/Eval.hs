-- If we got to this point, we know typechecking succeeded, so we can use incomplete patterns
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

module Eval(allSteps, eval, ProgState(..), showSteps, showOut) where

import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict(HashMap)
import Data.List(intercalate)

import AST

data VarState
  = Undefined
  | Defined Val

instance Show VarState where
  show Undefined = "!"
  show (Defined v) = show v

type SymValTable = HashMap Ident VarState
type ToDo = [Stmt]
type Out = [String]

data ProgState =
  ProgState
  { toDo :: ToDo
  , sym :: SymValTable
  , out :: Out
  }

instance Show ProgState where
  show ProgState{..} = unlines ["toDo = " ++ show toDo, "sym = " ++ sym', "out = " ++ show out]
    where
      sym' = "{ " ++ intercalate ", " (showVar <$> M.toList sym) ++ " }"
      showVar (ident, var) = ident ++ " <- " ++ show var

mkProgState :: Stmt -> ProgState
mkProgState stmt = ProgState { toDo = [stmt], sym = M.empty, out = [] }


showSteps :: [ProgState] -> String
showSteps = unlines . (show <$>)

showOut :: ProgState -> String
showOut = unlines . reverse . out

data EvalError
  = UninitializedVar Ident
  | DivisionByZero

instance Show EvalError where
  show te = "EvalError: " ++ go te ++ "."
    where
      go (UninitializedVar ident) = "Variable " ++ ident ++ " was declared, but not initialized"
      go DivisionByZero = "An attempt was made to divide by zero"

type Eval a = Either EvalError a

lookupVar :: Ident -> SymValTable -> Eval Val
lookupVar var sym =
  case sym M.! var of
    Undefined -> throw $ UninitializedVar var
    Defined v -> pure v

evalExpr :: SymValTable -> Expr -> Eval Val
evalExpr _ (Lit v) = pure v
evalExpr sym (Var ident) = lookupVar ident sym
evalExpr sym (Arith a op b) = do
  a' <- evalExpr sym a
  b' <- evalExpr sym b
  case (a', b') of
    (VInt va, VInt vb) ->
      if vb == 0 && (op == Divide || op == Remainder)
        then throw $ DivisionByZero
        else pure $ VInt $ arithOp op va vb
evalExpr sym (Logic a op b) = do
  a' <- evalExpr sym a
  b' <- evalExpr sym b
  case (a', b') of
    (VBool va, VBool vb) -> pure $ VBool $ logicOp op va vb
evalExpr sym (Comp a op b) = do
  va <- evalExpr sym a
  vb <- evalExpr sym b
  case (va, vb) of
    (VInt a', VInt b') -> pure $ VBool $ compOp op a' b'
    (VBool a', VBool b') -> pure $ VBool $ compOp op a' b'

evalStmt :: ProgState -> Stmt -> Eval ProgState
evalStmt progState Nop = pure progState
evalStmt ProgState{..} (Decl ident _) =
  pure $ ProgState {sym = M.insert ident Undefined sym, .. }
evalStmt ProgState{..} (Assign ident expr) = do
  v <- evalExpr sym expr
  pure $ ProgState {sym =  M.insert ident (Defined v) sym, .. }
evalStmt ProgState{..} (Print expr) = do
  v <- evalExpr sym expr
  pure $ ProgState { out = show v : out, .. }
evalStmt ProgState{..} (If cond then' else') = do
  c' <- evalExpr sym cond
  case c' of
    VBool c -> pure $ ProgState { toDo = (if c then then' else else') : toDo, .. }
evalStmt ProgState{..} w@(While cond body) = do
  c' <- evalExpr sym cond
  case c' of
    VBool c -> pure $ ProgState { toDo = if c then body : w : toDo else toDo, .. }
evalStmt ProgState{..} (Compound a b) =
  pure $ ProgState { toDo = a : b : toDo, .. }

smallStep :: ProgState -> Maybe (Eval ProgState)
smallStep ProgState { toDo = [] } = Nothing
smallStep ProgState { toDo = stmt : toDo, .. } = Just $ evalStmt ProgState{..} stmt

allSteps' :: ProgState -> Eval [ProgState]
allSteps' state =
  case smallStep state of
    Nothing -> pure [state]
    Just result -> do
      state' <- result
      states <- allSteps' state'
      pure $ state : states

allSteps :: Program -> TLI [ProgState]
allSteps prog = toTLI $ allSteps' $ mkProgState prog

eval :: Program -> TLI ProgState
eval p = last <$> allSteps p
