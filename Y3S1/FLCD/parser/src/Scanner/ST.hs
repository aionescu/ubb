module Scanner.ST(ST, IT, CT, emptyST, findOrInsert, lookupSymbol) where

import Control.Applicative((<|>))
import Prelude hiding (lookup)
import Data.Functor (($>))
import Control.Monad (guard)

-- Color to mark Tree nodes
data Color = R | B

instance Show Color where
  show R = "Red"
  show B = "Black"

-- Red-Black Tree data structure
data Tree k v
  = Leaf
  | Branch Color (Tree k v) k v (Tree k v)

instance (Show k, Show v) => Show (Tree k v) where
  show = showTree 0

showTree :: (Show k, Show v) => Int -> Tree k v -> String
showTree i Leaf = replicate i ' ' <> "Leaf\n"
showTree i (Branch c l k v r) =
  replicate i ' ' <> show c <> " (" <> show k <> ", " <> show v <> ")\n" <>
  showTree (i + 2) l <>
  showTree (i + 2) r

-- Helpers to make pattern matching easier
pattern Red :: Tree k v -> k -> v -> Tree k v -> Tree k v
pattern Red l k v r <- Branch R l k v r
  where
    Red l k v r = Branch R l k v r

pattern Black :: Tree k v -> k -> v -> Tree k v -> Tree k v
pattern Black l k v r <- Branch B l k v r
  where
    Black l k v r = Branch B l k v r

-- Lookup a value by its key
-- O(log n)
lookup :: Ord k => k -> Tree k v -> Maybe v
lookup _ Leaf = Nothing
lookup k (Branch _ l k' v r)
  | k < k' = lookup k l
  | k > k' = lookup k r
  | otherwise = Just v

-- Lookup a key by its value
-- O(n)
lookupValue :: Eq v => v -> Tree k v -> Maybe k
lookupValue _ Leaf = Nothing
lookupValue v (Branch _ l k v' r) = lookupValue v l <|> guard (v == v') $> k <|> lookupValue v r

-- Insert a key-value pair into the tree
-- O(log n)
insert :: Ord k => k -> v -> Tree k v -> Tree k v
insert key value tree = toBlack $ insert' tree
  where
    -- Ensure the root node is always black
    toBlack Leaf = Leaf
    toBlack (Branch _ l k v r) = Black l k v r

    -- Insert value and rebalance the tree
    insert' Leaf = Red Leaf key value Leaf
    insert' (Branch c l k v r)
      | key < k = balance $ Branch c (insert' l) k v r
      | key > k = balance $ Branch c l k v (insert' r)
      | otherwise = Branch c l k v r

    -- Balance a Tree node
    -- Because Red nodes cannot have other Red nodes as children,
    -- when a Red-Red subtree is found, it is rotated to be Red-Black.
    balance :: Tree k v -> Tree k v

    -- Left node, and its own left node, are Red
    balance (Black (Red (Red l'' k'' v'' r'') k' v' r') k v r) =
      Red (Black l'' k'' v'' r'') k' v' (Black r' k v r)

    -- Left node, and its right node, are Red
    balance (Black (Red l' k' v' (Red l'' k'' v'' r'')) k v r) =
      Red (Black l' k' v' l'') k'' v'' (Black r'' k v r)

    -- Right node, and its right node, are Red
    balance (Black l k v (Red (Red l'' k'' v'' r'') k' v' r')) =
      Red (Black l k v l'') k'' v'' (Black r'' k' v' r')

    -- Right node, and its own right node, are Red
    balance (Black l k v (Red l' k' v' (Red l'' k'' v'' r''))) =
      Red (Black l k v l') k' v' (Black l'' k'' v'' r'')

    -- Otherwise, return the tree unchanged
    balance t = t

-- Symbol Table data structure
data ST s =
  ST
  { tree :: Tree s Int
  , size :: Int
  }

instance Show s => Show (ST s) where
  show = show . tree

emptyST :: ST s
emptyST = ST Leaf 0

-- If the symbol exists, return its index and the unchanged symbol table.
-- Otherwise, insert it and return the new index and updated symbol table.
findOrInsert :: Ord s => s -> ST s -> (Int, ST s)
findOrInsert symbol st@ST{..} =
  case lookup symbol tree of
    Nothing -> (size, ST { tree = insert symbol size tree, size = size + 1 })
    Just ix -> (ix, st)

-- Lookup a symbol by its index.
lookupSymbol :: Int -> ST s -> Maybe s
lookupSymbol ix ST{..} = lookupValue ix tree

-- Identifier Table
type IT = ST String

-- Constant Table
type CT = ST Int
