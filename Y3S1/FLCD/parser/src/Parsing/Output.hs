module Parsing.Output where

import Control.Monad.State(State, put, get, execState)
import Data.List(intercalate)
import Data.Map qualified as M

import Grammar.Core
import Parsing.RecursiveDescent hiding (State)

data Node
  = Term Terminal
  | NonTerm String [Node]

instance Show Node where
  show (Term c) = show c
  show (NonTerm a ns) = a <> " [" <> intercalate ", " (show <$> ns) <> "]"

genNodes :: CFG -> Int -> [Rule] -> ([Node], [Rule])
genNodes g = go []
  where
    go :: [Node] -> Int -> [Rule] -> ([Node], [Rule])
    go ns 0 rs = (reverse ns, rs)
    go ns n rs = go (n' : ns) (n - 1) rs'
      where
        (n', rs') = genNode g rs

genNode :: CFG -> [Rule] -> (Node, [Rule])
genNode _ [] = error "genNode: Unreachable"
genNode _ (T c : rs) = (Term c, rs)
genNode g@Grammar{..} (NT a i : rs) = (NonTerm a ns, rs')
  where
    prod = ofProd $ productions M.! a !! (i - 1)
    n = length prod
    (ns, rs') = genNodes g n rs

genTree :: CFG -> [Rule] -> Node
genTree g rs = fst $ genNode g $ reverse rs

showTree :: Node -> String
showTree node = "Tree:\n" <> show node

type Table = [(String, Int, Int)]

toTable :: Node -> State (Int, Int, Int, Table) ()
toTable (Term c) = do
  (next, parent, sibling, t) <- get
  put (next + 1, parent, sibling + 1, (show c, parent, sibling) : t)
toTable (NonTerm a ns) = do
  (next, parent, sibling, t) <- get

  put (next + 1, next, 0, (a, parent, sibling) : t)
  mapM_ toTable ns

genTable :: CFG -> [Rule] -> Table
genTable g rs = reverse table
  where
    (_, _, _, table) = execState (toTable $ genTree g rs) (1, 0, 0, [])

showTable :: Table -> String
showTable tbl = intercalate "\n" $ header : (uncurry fmt <$> zip [1..] tbl)
  where
    header = "Index, Info, Parent, Right Sibling"

    fmt :: Int -> (String, Int, Int) -> String
    fmt i (info, parent, sibling) = intercalate ", " [show i, info, show parent, show sibling]
