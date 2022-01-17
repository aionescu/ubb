# L3 - Scanner

[_GitHub URI_](https://github.com/aionescu/flcd/tree/main/L3)

## Overview

The scanner accepts as input two files: a file containing the language's tokens, and the source file to scan.

It outputs 3 files:

* [`Out/PIF.out`](https://github.com/aionescu/flcd/blob/main/L3/Out/PIF.out): Containts the PIF obtained from scanning the source code
* [`Out/IT.out`](https://github.com/aionescu/flcd/blob/main/L3/Out/IT.out): Contains the Identifier Table, where scanned identifiers are stored
* [`Out/CT.out`](https://github.com/aionescu/flcd/blob/main/L3/Out/CT.out): Contains the Constant Table, where scanned constants are stored

## Implementation

The scanner is implemented in [Haskell](https://haskell.org/), and the source code can be found in the [src](https://github.com/aionescu/flcd/tree/main/L3/src/) folder.

The main data types are:

* `Token`: Stores information about a scanned token.
* `PIF`: The type of the resulting PIF. Synonym for `[Token]` (i.e. list of `Token`s)
* `ScanState`: Stores the state of the scanner at a given moment, including the current PIF and symbol tables, as well as line and column numbers and the remaining input.
* `Scanner`: The monad used for scanning. More info in a [later section](#the-scanner-monad).

The main functions are:

* `skipWS`: Trims all whitespace from the beginning of the input, updating the line and column numbers accordingly.
* `detect`: Reads the first token from the input and attempts to categorize it into either a `Token`, `Id` or `Const`.
* `scan`: The "scan loop", repeatedly calls `skipWS` and `detect` until `EOF` is reached.
* `runScanner`: Wrapper function for `scan`, which abstracts away the creation of the `ScanState` and the `Scanner` monad.

The used RegEx patterns are:

* Identifiers (`[a-zA-Z_][a-zA-Z0-9_]*`): Identifiers must begin with a letter or underscore, and then contain letters, digits, and underscores.
* Integer Constants (`0|((-|\\+)?[1-9][0-9]*)`): Integer constants are either just `0`, or an optional sign followed by a non-zero digit and 0 or more possibly-zero digits.

## The `Scanner` Monad

[Monads](https://en.wikipedia.org/wiki/Monad_(functional_programming)) are the mechanism used to manage side-effects in Haskell.

Scanning is performed inside the `Scanner` monad, which is a [monad transformer](https://en.wikipedia.org/wiki/Monad_transformer) stack consisting of:

* `Reader`: To pass the list of known tokens in a read-only way
* `State`: To store the state of the scanner
* `Except`: For handling scanning errors
