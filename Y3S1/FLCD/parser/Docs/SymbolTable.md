# L2 - Symbol Table Implementation

[*GitHub Repo*](https://github.com/aionescu/flcd/tree/main/L2)

## Symbol Table Organization

The Symbol Table is implemented as a self-balancing binary search tree (in particular, a [Red-Black tree](https://en.wikipedia.org/wiki/Red%E2%80%93black_tree)).

Each node in the tree stores a `(symbol, index)` pair, where the `symbol` determines the sorting of the tree.

## Exposed Data Types

* `Tree k v` - Red-Black tree with keys of type `k` and values of type `v`
* `ST s` - Symbol table for symbols of type `s`. Internally stores a `Tree s Int`, and a counter for the next symbol's index
* `IT` - Identifier Table. Synonym for `ST String`
* `CT` - Constant Table. Synonym for `ST Int`

## Symbol Table Operations

```haskell
findOrInsert :: s -> ST s -> (Int, ST s)
```

Searches for the specified symbol in the Symbol Table.

If it is found, its index is returned, along with the unchanged Symbol Table.

If it is not found, it is inserted, and its new index is returned along with the modified Symbol Table.

Complexity: `O(log n)`

## Tree Operations

```haskell
insert :: k -> v -> Tree k v -> Tree k v
```

Inserts the specified key-value pair into the tree, rebalancing the tree if needed.

Complexity: `O(log n)`

---

```haskell
lookup :: k -> Tree k v -> Maybe v
```

Searches the tree for the value associated associated with the specified key.

Complexity: `O(log n)`

---

```haskell
lookupValue :: v -> Tree k v -> Maybe k
```

Searches the tree for the key associated associated with the specified value.

Complexity: `O(n)`
