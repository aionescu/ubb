module Language.Xi.TypeChecker(typeCheck) where

import Control.Monad(join, when)
import Control.Monad.Except(liftEither, throwError, MonadError)
import Control.Monad.Fix(MonadFix)
import Control.Monad.Reader(ReaderT, runReaderT, local, ask, MonadReader)
import Data.Bifunctor(first, second)
import Data.Functor(($>))
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import Data.Maybe(isJust)

import Language.Xi.Syntax

type Env = Map Ident Type

data TypeError
  = ExpectedFound Type Type
  | ExpectedSubtypeFound Type Type
  | UndeclaredVar Ident
  | ExpectedRecFound Type
  | ExpectedTupFound Type
  | NoFieldInRec Type Ident
  | TupTooShort Type Int
  | NeedRecForIntersect
  | NeedTyRecForIntersect
  | ExpectedFunFound Type
  | TypeIsOpaque Type
  | CanOnlyAppendStrings
  | CanOnlyAddIntegers
  | ExpectedRefFound Type
  | CantBeRecursive
  | DupTyRecField TyExpr
  | CantShadowIntrinsic Ident

instance Show TypeError where
  show te = "Type error: " ++ go te ++ "."
    where
      go (ExpectedFound expected found) = "Expected " ++ show expected ++ ", but found " ++ show found
      go (ExpectedSubtypeFound expected found) = "Expected a subtype of " ++ show expected ++ ", but found " ++ show found
      go (UndeclaredVar ident) = "Variable \"" ++ ident ++ "\" is not in scope"
      go (ExpectedRecFound t) = "Expected record type, but found " ++ show t
      go (ExpectedTupFound t) = "Expected tuple type, but found " ++ show t
      go (NoFieldInRec t i) = "The record type " ++ show t ++ " has no field named " ++ i
      go (TupTooShort t i) = "The tuple type " ++ show t ++ " does not have enough elements to be indexed by the index " ++ show i
      go NeedRecForIntersect = "Both operands of the \"&\" operator must be records"
      go NeedTyRecForIntersect = "Only record types can be intersected"
      go (ExpectedFunFound t) = "Expected function type, but found " ++ show t
      go (TypeIsOpaque t) = "The type " ++ show t ++ " is opaque"
      go CanOnlyAppendStrings = "Both operands of the append operation must be strings"
      go CanOnlyAddIntegers = "Both operands of the addition operation must be integers"
      go (ExpectedRefFound t) = "Expected reference type, but found type " ++ show t
      go CantBeRecursive = "Only return-type-annotated lambda bindings can be recursive"
      go (DupTyRecField t) = "Duplicate record field in type " ++ show t
      go (CantShadowIntrinsic t) = "Cannot shadow intrinsic type " ++ t

isOpaque :: Type -> Bool
isOpaque (Ref _) = True
isOpaque (_ :-> _) = True
isOpaque (Rec m) = any isOpaque m
isOpaque (Tup ts) = any isOpaque ts
isOpaque _ = False

intersect :: MonadError TypeError m => Type -> Type -> m Type
intersect (Rec as) (Rec bs) = pure $ Rec $ M.union bs as
intersect _ _ = throwError NeedRecForIntersect

normalize :: (MonadError TypeError m, MonadReader Env m) => TyExpr -> m Type
normalize (TyVar i) = ask >>= lookupVar i
normalize (TyIntersect a b) = join $ intersect <$> normalize a <*> normalize b
normalize (TyFn i o) = (:->) <$> normalize i <*> normalize o

normalize t@(TyRec m) =
  case sequenceA $ M.fromListWith dedup $ second Just <$> m of
    Nothing -> throwError $ DupTyRecField t
    Just m' -> Rec <$> traverse normalize m'
  where
    dedup _ _ = Nothing

normalize (TyTup ts) = Tup <$> traverse normalize ts
normalize (TyRef t) = Ref <$> normalize t

isSubtypeOf :: Type -> Type -> Bool
isSubtypeOf a b | a == b = True
isSubtypeOf (Tup as) (Tup bs) = length as == length bs && and (zipWith isSubtypeOf as bs)
isSubtypeOf (Rec as) (Rec bs) = M.null (bs M.\\ as) && and (M.intersectionWith isSubtypeOf as bs)
isSubtypeOf (a :-> b) (a' :-> b') = a' `isSubtypeOf` a && b `isSubtypeOf` b'
isSubtypeOf _ _ = False

unwrapRef :: MonadError TypeError m => Type -> m Type
unwrapRef (Ref t) = pure t
unwrapRef t = throwError $ ExpectedRefFound t

mustBe :: MonadError TypeError m => Type -> Type -> m ()
mustBe found expected
  | found == expected = pure ()
  | otherwise = throwError $ ExpectedFound expected found

mustBeSubtypeOf :: MonadError TypeError m => Type -> Type -> m ()
mustBeSubtypeOf found expected
  | found `isSubtypeOf` expected = pure ()
  | otherwise = throwError $ ExpectedSubtypeFound expected found

upCast :: Type -> Type -> Expr Type -> Expr Type
upCast to from (e `As` _) = upCast to from e
upCast to from e | to == from = e
upCast to _ e = e `As` to

lookupVar :: MonadError TypeError m => Ident -> Map Ident a -> m a
lookupVar var sym = maybe (throwError $ UndeclaredVar var) pure $ M.lookup var sym

typeCheck' :: (MonadFix m, MonadError TypeError m, MonadReader Env m) => Expr TyExpr -> m (Type, Expr Type)
typeCheck' (Lit n@NumLit{}) = pure (Num, Lit n)
typeCheck' (Lit b@BoolLit{}) = pure (Bool, Lit b)
typeCheck' (Lit s@StrLit{}) = pure (Str, Lit s)

typeCheck' (Var i) = do
  t <- lookupVar i =<< ask
  pure (t, Var i)

typeCheck' (Arith a Add b) = do
  (ta, ea) <- typeCheck' a
  (tb, eb) <- typeCheck' b

  case (ta, tb) of
    (Num, Num) -> pure (Num, Arith ea Add eb)
    (Str, Str) -> pure (Str, Arith ea Add eb)
    _ ->
      throwError
        if Str `elem` [ta, tb]
        then CanOnlyAppendStrings
        else CanOnlyAddIntegers

typeCheck' (Arith a op b) = do
  (ta, ea) <- typeCheck' a
  ta `mustBe` Num
  (tb, eb) <- typeCheck' b
  tb `mustBe` Num
  pure (Num, Arith ea op eb)

typeCheck' (Logic a op b) = do
  (ta, ea) <- typeCheck' a
  ta `mustBe` Bool
  (tb, eb) <- typeCheck' b
  tb `mustBe` Bool
  pure (Bool, Logic ea op eb)

typeCheck' (Comp a op b) = do
  (ta, ea) <- typeCheck' a
  (tb, eb) <- typeCheck' b
  tb `mustBe` ta

  when (isOpaque ta) $
    throwError $ TypeIsOpaque ta

  pure (Bool, Comp ea op eb)

typeCheck' (Not e) = do
  (t, e') <- typeCheck' e
  t `mustBe` Bool
  pure (Bool, Not e')

typeCheck' (RecLit m) = do
  (is, ts, es) <- unzip3 . fmap reassoc <$> traverse (traverse typeCheck') m
  let t = Rec $ M.fromList $ zip is ts
  let e = RecLit $ zip is es
  pure (t, e)
    where
      reassoc (a, (b, c)) = (a, b, c)

typeCheck' (TupLit es) = do
  (ts, es') <- unzip <$> traverse typeCheck' es
  pure (Tup ts, TupLit es')

typeCheck' (RecMember lhs i) = do
  (t, lhs') <- typeCheck' lhs
  case t of
    Rec m ->
      case M.lookup i m of
        Nothing -> throwError $ NoFieldInRec t i
        Just t' -> pure (t', RecMember lhs' i)
    _ -> throwError $ ExpectedRecFound t

typeCheck' (TupMember lhs i) = do
  (t, lhs') <- typeCheck' lhs
  case t of
    Tup ts ->
      case ts !? i of
        Nothing -> throwError $ TupTooShort t i
        Just t' -> pure (t', TupMember lhs' i)
    _ -> throwError $ ExpectedTupFound t

typeCheck' (Intersect a b) = do
  (ta, ea) <- typeCheck' a
  (tb, eb) <- typeCheck' b
  case (ta, tb) of
    (Rec as, Rec bs) -> pure (Rec $ M.union bs as, Intersect ea eb)
    _ -> throwError NeedRecForIntersect

typeCheck' (Lam i ty e) = do
  ti <- normalize ty
  (to, e') <- local (M.insert i ti) (typeCheck' e)
  pure (ti :-> to, Lam i ti e')

typeCheck' (App f a) = do
  (tf, ef) <- typeCheck' f
  (ta, ea) <- typeCheck' a
  case tf of
    i :-> o -> ta `mustBeSubtypeOf` i $> (o, App ef $ upCast i ta ea)
    _ -> throwError $ ExpectedFunFound tf

typeCheck' (Deref e) = do
  (t, e') <- typeCheck' e
  t' <- unwrapRef t
  pure (t', Deref e')

typeCheck' (Print e) = do
  (t, e') <- typeCheck' e

  when (isOpaque t) $
    throwError $ TypeIsOpaque t

  pure (unit, Print e')

typeCheck' (If cond then' else') = do
  (tc, ec) <- typeCheck' cond
  tc `mustBe` Bool

  (tThen, eThen) <- typeCheck' then'
  (tElse, eElse) <- typeCheck' else'

  tElse `mustBe` tThen
  pure (tThen, If ec eThen eElse)

typeCheck' (RefExpr e) = do
  (t, e') <- typeCheck' e
  pure (Ref t, RefExpr e')

typeCheck' (Assign lhs rhs) = do
  (tl, el) <- typeCheck' lhs
  tl' <- unwrapRef tl

  (tr, er) <- typeCheck' rhs
  tr `mustBeSubtypeOf` tl'

  pure (unit, Assign el (upCast tl' tr er))

typeCheck' (Seq a b) = do
  (ta, ea) <- typeCheck' a
  ta `mustBe` unit

  (tb, eb) <- typeCheck' b
  pure (tb, Seq ea eb)

typeCheck' (Let False i Nothing v e) = do
  (tv, ev) <- typeCheck' v

  (te, ee) <- local (M.insert i tv) $ typeCheck' e
  pure (te, Let False i Nothing ev ee)

typeCheck' (Let False i (Just ty) v e) = do
  t <- normalize ty
  (tv, ev) <- typeCheck' v

  tv `mustBeSubtypeOf` t

  (te, ee) <- local (M.insert i tv) $ typeCheck' e
  pure (te, Let False i (Just t) (upCast t tv ev) ee)

typeCheck' (Let True _ Nothing _ _) = throwError CantBeRecursive

typeCheck' (Let True i (Just ty) v e) = mdo
  case v of
    Lam{} -> pure ()
    _ -> throwError CantBeRecursive

  t <- normalize ty
  (tv, ev) <- local (M.insert i t) $ typeCheck' v

  tv `mustBeSubtypeOf` t

  (te, ee) <- local (M.insert i t) $ typeCheck' e
  pure (te, Let True i (Just t) (upCast t tv ev) ee)

typeCheck' (LetTy i ty e) = do
  when (isJust $ M.lookup i intrinsicTypes) $
    throwError $ CantShadowIntrinsic i

  t <- normalize ty
  local (M.insert i t) $
    typeCheck' e

typeCheck' (e `As` ty) = do
  t <- normalize ty
  (t', e') <- typeCheck' e

  t' `mustBeSubtypeOf` t
  pure (t, upCast t t' e')

runTC :: MonadError String m => ReaderT Env (Either TypeError) a -> m a
runTC m = liftEither (first show $ runReaderT m intrinsicTypes)

typeCheck :: MonadError String m => Expr TyExpr -> m (Expr Type)
typeCheck prog = runTC do
  (t, e) <- typeCheck' prog
  t `mustBe` unit
  pure e
