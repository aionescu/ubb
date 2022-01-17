module Language.Xi.Eval(eval) where

import Control.Monad.Fix(MonadFix)
import Control.Monad.IO.Class(MonadIO(liftIO))
import Control.Monad.Reader(asks, MonadReader(local), ReaderT(runReaderT))
import Data.Functor(($>))
import Data.IORef(writeIORef, newIORef, readIORef, IORef)
import Data.List (intercalate)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M

import Language.Xi.Syntax

panic :: String -> a
panic msg = error $ "Panicked on \"" ++ msg ++ "\"."

type Env = Map Ident Val

data Val
  = VNum Integer
  | VBool Bool
  | VStr String
  | VRec (Map Ident Val)
  | VTup [Val]
  | VFn Env Ident (Expr Type)
  | VRef (IORef Val)

instance Show Val where
  show (VNum n) = show n
  show (VBool b) = show b
  show (VStr s) = show s
  show (VRec m) | M.null m = "{ }"
  show (VRec m) = "{ " ++ intercalate ", " (uncurry (showField " = ") <$> M.toList m) ++ " }"
  show (VTup [t]) = "(" ++ show t ++ ",)"
  show (VTup ts) = "(" ++ intercalate ", " (show <$> ts) ++ ")"
  show _ = panic "show opaque"

instance Ord Val where
  compare (VNum a) (VNum b) = compare a b
  compare (VBool a) (VBool b) = compare a b
  compare (VStr a) (VStr b) = compare a b
  compare (VRec a) (VRec b) = compare a b
  compare (VTup a) (VTup b) = compare a b
  compare _ _ = panic "compare opaque"

instance Eq Val where
  (==) = ((== EQ) .) . compare

vUnit :: Val
vUnit = VTup []

upCast :: Type -> Val -> Val
upCast (Tup ts) (VTup vs) = VTup $ zipWith upCast ts vs
upCast (Rec ts) (VRec vs) = VRec $ M.intersectionWith upCast ts vs
upCast (_ :-> t) (VFn env i e) = VFn env i $ e `As` t
upCast _ v = v

eval' :: (MonadFix m, MonadReader Env m, MonadIO m) => Expr Type -> m Val
eval' (Lit (NumLit n)) = pure $ VNum n
eval' (Lit (BoolLit b)) = pure $ VBool b
eval' (Lit (StrLit s)) = pure $ VStr s

eval' (Var i) = asks (M.! i)

eval' (Arith a op b) = do
  a' <- eval' a
  b' <- eval' b
  case (a', op, b') of
    (VStr va, Add, VStr vb) -> pure $ VStr $ va ++ vb
    (VNum va, _, VNum vb) -> pure $ VNum $ arithOp op va vb
    _ -> panic "Arith"

eval' (Logic a op b) = do
  a' <- eval' a
  case (a', op) of
    (VBool True, Or) -> pure a'
    (VBool False, And) -> pure a'
    (VBool va, _) -> do
      b' <- eval' b
      case b' of
        VBool vb -> pure $ VBool $ logicOp op va vb
        _ -> panic "Logic RHS"
    _ -> panic "Logic"

eval' (Comp a op b) = do
  a' <- eval' a
  b' <- eval' b
  pure $ VBool $ compOp op a' b'

eval' (Not e) = do
  v <- eval' e
  case v of
    VBool b -> pure $ VBool $ not b
    _ -> panic "Not"

eval' (RecLit m) = VRec . M.fromList <$> traverse (traverse eval') m
eval' (TupLit es) = VTup <$> traverse eval' es

eval' (RecMember lhs i) = do
  v <- eval' lhs
  case v of
    VRec m -> pure $ m M.! i
    _ -> panic "RecMember"

eval' (TupMember lhs i) = do
  v <- eval' lhs
  case v of
    VTup vs ->  pure $ vs !! i
    _ -> panic "TupMember"

eval' (Intersect a b) = do
  ra <- eval' a
  rb <- eval' b
  case (ra, rb) of
    (VRec a', VRec b') -> pure $ VRec $ M.union b' a'
    _ -> panic "Intersect"

eval' (Lam i _ e) = asks \env -> VFn env i e

eval' (App f a) = do
  vf <- eval' f
  va <- eval' a
  case vf of
    VFn env' i e ->
      local (const $ M.insert i va env') $
        eval' e
    _ -> panic "App"

eval' (Deref e) = do
  v <- eval' e
  case v of
    VRef ioRef -> liftIO $ readIORef ioRef
    _ -> panic "Deref"

eval' (Print expr) = do
  v <- eval' expr
  liftIO $ print v
  pure vUnit

eval' (If cond then' else') = do
  c' <- eval' cond
  case c' of
    VBool c -> eval' if c then then' else else'
    _ -> panic "If"

eval' (RefExpr e) = do
  v <- eval' e
  VRef <$> liftIO (newIORef v)

eval' (Assign lhs rhs) = do
  vl <- eval' lhs
  vr <- eval' rhs
  case vl of
    VRef ioRef -> liftIO (writeIORef ioRef vr) $> vUnit
    _ -> panic "Assign`"

eval' (Let False i _ v e) = do
  v' <- eval' v
  local (M.insert i v') $
    eval' e

eval' (Let True i _ v e) = mdo
  v' <- local (M.insert i v') $ eval' v

  local (M.insert i v') $
    eval' e

eval' (Seq a b) = eval' a *> eval' b

eval' LetTy{} = panic "LetTy"

eval' (e `As` t) = upCast t <$> eval' e

runEval :: ReaderT Env IO a -> IO ()
runEval m = runReaderT m M.empty $> ()

eval :: Expr Type -> IO ()
eval = runEval . eval'
