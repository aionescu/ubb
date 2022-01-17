module Parsing.RecursiveDescent where

import Data.List(isPrefixOf)
import Data.Map qualified as M

import Grammar.Core
import Scanner.Core

data State = Q | B | F | E
  deriving stock (Eq, Ord, Show)

data Terminal
  = TId
  | TConst
  | TToken String
  deriving stock (Eq, Ord)

instance Show Terminal where
  show TId = "ID"
  show TConst = "CONST"
  show (TToken s) = show s

ofToken :: Token -> Terminal
ofToken (Token s) = TToken s
ofToken (Id _) = TId
ofToken (Const _) = TConst

data Rule
  = T Terminal
  | NT String Int
  deriving stock (Eq, Ord)

instance Show Rule where
  show (T c) = show c
  show (NT s i) = s <> show i

ofAtom :: Atom -> Rule
ofAtom (NonTerminal nt) = NT nt 1
ofAtom (Terminal t) = T $ TToken t
ofAtom BuiltinId = T TId
ofAtom BuiltinConst = T TConst

ofProd :: Prod -> [Rule]
ofProd Eps = []
ofProd (Prod as) = ofAtom <$> as

type Config = (State, Int, [Rule], [Rule])

initialConfig :: CFG -> Config
initialConfig Grammar{..} = (Q, 1, [], [NT initialProd 1])

move :: a -> b -> (b, a)
move = flip (,)

step :: CFG -> [Terminal] -> Config -> (String, Config)
step g@Grammar{..} input config =
  case config of
    (Q, i, α, NT a _ : β) -> (Q, i, NT a 1 : α, γ <> β) `move` "Expand"
      where
        γ = ofProd $ head $ productions M.! a

    (Q, i, α, T a : β)
      | Just a == (input !? (i - 1)) -> (Q, i + 1, T a : α, β) `move` "Advance"
      | otherwise -> (B, i, α, T a : β) `move` "Momentary insuccess"

    (B, i, T a : α, β) -> (B, i - 1, α, T a : β) `move` "Back"

    (B, i, NT a j : α, matchProd g a j -> Just β)
      | Just γ <- nextProd g a j -> (Q, i, NT a (j + 1) : α, γ <> β) `move` "Another try Good"

      | i == 1
      , a == initialProd -> (E, i, α, β) `move` "Another try Error"

      | otherwise -> (B, i, α, NT a 1 : β) `move` "Another try Back"

    (Q, i, α, []) | i == length input + 1 -> (F, i, α, []) `move` "Success"

    state -> error $ "step: Unreachable " <> show state

nextProd :: CFG -> String -> Int -> Maybe [Rule]
nextProd Grammar{..} a j = (ofProd <$>) . (!? j) =<< productions M.!? a

matchProd :: CFG -> String -> Int -> [Rule] -> Maybe [Rule]
matchProd Grammar{..} a j β =
  productions M.!? a >>= \prods ->
    case prods !! (j - 1) of
      Eps -> Just β
      Prod as ->
        if (ofAtom <$> as) `isPrefixOf` β
        then Just $ drop (length as) β
        else Nothing

recursiveDescent :: CFG -> PIF -> [(Config, String)]
recursiveDescent g pif = go $ initialConfig g
  where
    input = ofToken <$> pif

    go c@(F, _, _, _) = [(c, "Success")]
    go c@(E, _, _, _) = [(c, "Error")]
    go c = (c, mv) : go next
      where
        (mv, next) = step g input c
