module TypeCk(typeCheck) where

import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict(HashMap)

import AST

type SymTypeTable = HashMap Ident Type

data TypeError
  = ExpectedFound Type Type
  | UndeclaredVar Ident
  | VarAlreadyDeclared Ident

instance Show TypeError where
  show te = "TypeError: " ++ go te ++ "."
    where
      go (ExpectedFound expected found) = "Expected " ++ show expected ++ ", but found " ++ show found
      go (UndeclaredVar ident) = "Variable " ++ ident ++ "was not declared"
      go (VarAlreadyDeclared ident) = "Variable " ++ ident ++ "has already been declared"

type TypeCk a = Either TypeError a

mustBe :: Type -> Type -> TypeCk ()
mustBe found expected
  | found == expected = pure ()
  | otherwise = throw $ ExpectedFound expected found

valType :: Val -> Type
valType (VBool _) = TBool
valType (VInt _) = TInt

lookupVar :: Ident -> SymTypeTable -> TypeCk Type
lookupVar var sym = maybe (throw $ UndeclaredVar var) pure $ M.lookup var sym

typeCheckExpr :: SymTypeTable -> Expr -> TypeCk Type
typeCheckExpr _ (Lit v) = pure $ valType v
typeCheckExpr sym (Var ident) = lookupVar ident sym
typeCheckExpr sym (Arith a _ b) = do
  ta <- typeCheckExpr sym a
  ta `mustBe` TInt
  tb <- typeCheckExpr sym b
  tb `mustBe` TInt
  pure TInt
typeCheckExpr sym (Logic a _ b) = do
  ta <- typeCheckExpr sym a
  ta `mustBe` TBool
  tb <- typeCheckExpr sym b
  tb `mustBe` TBool
  pure TBool
typeCheckExpr sym (Comp a _ b) = do
  ta <- typeCheckExpr sym a
  tb <- typeCheckExpr sym b
  tb `mustBe` ta
  pure TBool

typeCheckStmt :: SymTypeTable -> Stmt -> TypeCk SymTypeTable
typeCheckStmt sym Nop = pure sym
typeCheckStmt sym (Decl ident type') =
  case M.lookup ident sym of
    Just _ -> throw $ VarAlreadyDeclared ident
    Nothing -> pure $ M.insert ident type' sym
typeCheckStmt sym (Assign ident expr) = do
  typ <- lookupVar ident sym
  typ2 <- typeCheckExpr sym expr
  typ2 `mustBe` typ
  pure sym
typeCheckStmt sym (DeclAssign ident type' expr) = do
  sym2 <- typeCheckStmt sym (Decl ident type')
  typeCheckStmt sym2 (Assign ident expr)
typeCheckStmt sym (Print e) = sym <$ typeCheckExpr sym e
typeCheckStmt sym (If cond then' else') = do
  tc <- typeCheckExpr sym cond
  tc `mustBe` TBool
  typeCheckStmt sym then'
  typeCheckStmt sym else'
  pure sym
typeCheckStmt sym (While cond body) = do
  tc <- typeCheckExpr sym cond
  tc `mustBe` TBool
  typeCheckStmt sym body
  pure sym
typeCheckStmt sym (Compound a b) = do
  sym2 <- typeCheckStmt sym a
  typeCheckStmt sym2 b

typeCheck :: Program -> TLI Program
typeCheck prog = toTLI $ prog <$ typeCheckStmt M.empty prog
