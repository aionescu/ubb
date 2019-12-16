# numeration-bases

*This is a Markdown-formatted version of the documentation. A PDF-rendered version of this document should have also been distributed along with the source code.*

This project implements conversions between numeration bases, as well as operations in arbitrary numeration bases (for bases between 2 and 16).

## Interface

The program uses a console-based interface that is meant to replicate the mathematical notation of the performed operations.

As such, in order to convert a number between two bases, the user should input `n(b) = ?(h)`, where `n` is the number to convert, `b` is the source base, and `h` is the destination base. (e.g. `2A(16) = ?(10)`)

In order to perform an arithmetic operation, the user should input `x(b) <op> y(b)`, where `<op>` is one of `+`, `-`, `*` or `/`, `x` and `y` are the operands, and `b` is the base in which the operation is performed. (e.g. `44(8) + 56(8)`)

## Operations

The following arithmetic operations are supported for any base between 2 and 16:

* Addition (e.g. `44(8) + 56(8)`)
* Subtraction (e.g. `5FA(16) - DD(16)`)
* Multiplication by 1 digit (e.g. `2A(12) * 2(12)`)
* Division by 1 digit (e.g. `3C(16) / 4(16)`)
  * Both the quotient and the remainder of the division are computed

## Conversions

The program supports multiple conversion methods: The substitution method, the method of successive divions, the method of using an intermediate base (using 10 as the intermediary base), and rapid conversions between bases that are powers of 2.

The program automatically infers which conversion method to use based on the source and destination base.

The used conversion method is displayed along with the result of the conversion.

## Implementation

The project is implemented in [Haskell](https://en.wikipedia.org/wiki/Haskell_(programming_language)), a purely functional programming language.
Because Haskell is a purely functional language, it does not support looping constructs. Their behaviour is achieved using recursive functions.

### Used data structures

In the program, numbers in an arbitrary base are represented as strings. In Haskell, strings are just linked lists of characters, and the recursive nature of linked lists makes using them with recursive algorithms very easy. Also, because lists in Haskell grow from the front, not the back, the numbers' digits are stored in reverse order.

In the `Operations.hs` file, we can see the definition of the data types used to represent numbers:

```hs
type Digit = Char
type Digits = String
type Base = Int
```

The types are just aliases for intrinsic Haskell types: `Digits` is the type of numbers with multiple digits, which is an alias for `String`, which itself is an alias for a list of characters, i.e. `[Char]`, and `Digit` is used for single-digit numbers, and is an alias for a single `Char`.

### Used algorithms

Let's see for example the implementation of multiplication, also taken from the `Operations.hs` file:

```hs
mul :: Base -> Digits -> Digit -> Digits
mul base x y = loop 0 x
  where
    b = y
    step carry a b =
      let i = carry + toBase10 a * toBase10 b
      in i `quotRem` base

    loop carry (a : as) =
      let (quot, rem) = step carry a b
      in toDigit rem : (loop quot as)

    loop 0 [] = []
    loop carry [] = [toDigit carry]
```

The first line represents the function's *type signature*. It specifies the type of its arguments, and its return type. In this case, the arguments are of types `Base`, `Digits`, and `Digit`, and the return type is `Digits`.

The next line is the definition of the function. Its arguments are given names (`base`, `x`, and `y`), and to the right of the equals sign is the *body* of the function, which is a call to the `loop` function, passing 0 and `x` as its parameters.

The `where` clause is used to specify a series of *bindings*, similar to local variables in other languages. The difference, however, is that unlike variables, bindings are *immutable*, their value cannot change once defined.

The first binding, `b = y`, simply defines a new name for the `y` argument. The following bindings are function definitions.

The `step` functions represents the computation that is performed on each step of the recursive "loop". It takes a carry and 2 numbers as arguments, and sums the carry with the product of the 2 numbers, then divides the result by the base. The `quotRem` function returns both the quotient and the remainder of the division as a pair. 

The next function, `loop`, is a recursive function that is used to achieve the purpose of a `while` loop in imperative programs. It takes two arguments: the carry from the previous iteration, as well as the rest of the number, which is *pattern matched* on: The syntax `(a : as)` decomposes the list into its *head* (the first element) and *tail* (the rest of its elements), and stores them in `a`, and `as`, respectively.

In the body of the `loop` function we can see that it applies the previously defined `step` function to the previous carry, the current digit of the first number, and the second number (which is guaranteed to be a single digit -- This is enforced by the language's strong type system: Notice in the type signature of `mul` that the first number is of type `Digits`, while the second is of type `Digit`).

The result of the `step` function is *destructured* into the `quot` and `rem` bindings, which are used in the function's result: the remainder is used as the next digit of the resulting number, while the quotient is passed as the carry in the next recursive invocation of the function.

The following two function definitions are also definitions for `loop`. They specify what should be executed in the case of an empty list, that is, when we are done looping through `a`'s digits. The first definition says that if the carry of the last iteration is 0, then the empty list (`[]`) is returned, but if the carry is non-zero, we need to add an extra digit to the result, which is the carry, converted to the correct base.

The functions for the other arithmetic operations, as well as for conversions between bases (which can be found in the `Conversions.hs` file), are implemented in a similar manner, using recursive "loop" functions.

### Parsing

The program takes input in a quite complex way: The user is able to input expressions containing numbers in arbitrary bases, and the programs deduces what operation or conversion it should compute based on that.

The method in which the program "understands" the given expressions is called `parsing`.

The implementation of the parser can be found in the `Parser.hs` file, and it uses a popular Haskell library called `Parsec`.