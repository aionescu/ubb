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

The program supports multiple conversion methods: The substitution method, the method of successive divions, and the method of using an intermediate base (using 10 as the intermediary base).

The program deduces which method to use based on the following condition: If both the source and destination base are lower than 10, then the method with an intermediary base 10 is chosen. Otherwise, if the source base is lower than the destination base, the substitution method is used. Otherwise, if the destination base is lower than the source base, the method of successive divisions is chosen. If the source and destination base are equal, no conversion is required.

## Implementation

The project is implemented in [Haskell](https://en.wikipedia.org/wiki/Haskell_(programming_language)), a purely functional programming language.
Because Haskell is a purely functional language, it does not support looping constructs. Their behaviour is achieved using recursive functions.