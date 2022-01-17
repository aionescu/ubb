module Grammar.Core where

import Data.Bitraversable(bitraverse)
import Data.List(intercalate)
import Data.Map(Map)
import Data.Map qualified as M
import Data.Maybe(mapMaybe)
import Data.Set(Set)
import Data.Set qualified as S

-- Datatypes

data Atom
  = BuiltinId
  | BuiltinConst
  | Terminal String
  | NonTerminal String
  deriving stock (Eq, Ord)

instance Show Atom where
  show BuiltinId = "ID"
  show BuiltinConst = "CONST"
  show (Terminal c) = show c
  show (NonTerminal s) = s

data Prod
  = Prod [Atom]
  | Eps
  deriving stock (Eq, Ord)

instance Show Prod where
  show Eps = "ϵ"
  show (Prod as) = unwords $ show <$> as

data Grammar a =
  Grammar
  { productions :: Map a [Prod]
  , initialProd :: String
  }

instance Show a => Show (Grammar a) where
  show Grammar{..} = unlines (uncurry fmt <$> M.toList productions) <> initialProd
    where
      fmt a ps = show a <> " -> " <> intercalate " | " (show <$> ps) <> ";"

type CFG = Grammar String

-- Operations

nonTerminals :: Grammar Prod -> Set String
nonTerminals Grammar{..} = S.fromList $ prodNonTerminals =<< uncurry (:) =<< M.toList productions
  where
    prodNonTerminals Eps = []
    prodNonTerminals (Prod atoms) = mapMaybe atomNonTerminal atoms

    atomNonTerminal (NonTerminal nt) = Just nt
    atomNonTerminal _ = Nothing

terminals :: Grammar Prod -> Set String
terminals Grammar{..} = S.fromList $ prodTerminals =<< uncurry (:) =<< M.toList productions
  where
    prodTerminals Eps = []
    prodTerminals (Prod atoms) = atomTerminals =<< atoms

    atomTerminals (Terminal t) = [t]
    atomTerminals _ = []

checkCFG :: Grammar Prod -> Maybe CFG
checkCFG Grammar{..} = flip Grammar initialProd . M.fromList <$> traverse (bitraverse checkLHS Just) (M.toList productions)
  where
    checkLHS lhs
      | Prod [NonTerminal nt] <- lhs = Just nt
      | otherwise = Nothing

hasLeftRecursion :: CFG -> Bool
hasLeftRecursion Grammar{..} = any (uncurry hasLeftRec) $ M.toList productions
  where
    hasLeftRec a = any (isLeftRec a)

    isLeftRec a (Prod (NonTerminal a' : _)) | a == a' = True
    isLeftRec _ _ = False

-- Helper functions

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)
infixr 8 ...

(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(a : as) !? i
  | i == 0 = Just a
  | i < 0 = Nothing
  | otherwise = as !? (i - 1)
infixl 5 !?
