# L4 - Finite Automata

[*Jump to GitHub repo*](https://github.com/aionescu/flcd/tree/L4)

## Important modules

* [`FA.Core`](src/FA/Core.hs): Contains the `FA` (Finite Automata) datatype and the `check` function (which checks whether a sequence is accepted by the FA)
* [`FA.Parser`](src/FA/Parser.hs): Contains helper datatypes and functions for parsing the FA from a file. The EBNF description of the format can be found [here](Data/FA.ebnf), and is also transcribed below.

## FA Representation

The data structure used to represent finite automata is the following:

```haskell
data FA =
  FA
  { faStates :: Set State
  , faInitial :: State
  , faFinal :: Set State
  , faAlphabet :: Set Char
  , faTransitions :: Map (State, Char) [State]
  }
```

It consists of a `Set` of all states, an initial state, a set of final states, a set of characters for the alphabet, and a mapping from (state, character) pairs to lists of states, representing transitions. Because transitions map to *lists* of states, NFAs [are also supported](Data/01.in).

## Sequence checking

2 different checking functions are implemented: `checkPrefix`, which returns the longest prefix of the sequence that is accepted by the FA, and `check`, which checks that the *entire* sequence is accepted.

### `checkPrefix`

The `checkPrefix` function starts with the FA's initial state, and iterates over the sequence as long as there exist transitions for each pair `(state, nextCharacter)`.

If the end of the sequence is reached, the current state is checked: If it is among the FA's final states, then the entire sequence is accepted. Otherwise, an error is reported.

If the end of the sequence was *not* reached, but a character without any transitions was found, the current state is also checked: If it is among the FA's final states, then the prefix of the sequence is accepted by the FA, and a `(prefix, restOfInput)` pair is returned. Otherwise, the entire sequence is invalid and an error is reported.

### `check`

The `check` function simply calls `checkPrefix`, and further checks that all of the input was accepted, i.e. that the returned tuple was of the form `(prefix, "")`.

## Finite Automata - EBNF specification

Below is the EBNF specification for the format used to read FAs from files.

```ebnf
state = {"A" | "B" | ... | "Z"}+
initial = "->" state
final = state "->"
transition = state "-[" {ANY_CHAR}+ "]->" state
stmt = initial | final | transition
fa = {stmt}+
```

The `{RULE}+` syntax was used to represent `1 or more`, in order to avoid the repetition of `RULE{RULE}`.

For some example automata, check out the [automaton for identifiers](Data/Id.in), or the one for [constants](Data/Int.in).
