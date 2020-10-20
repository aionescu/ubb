# tli

Toy Language Interpreter

## Building & running

To build the project, you will need `cabal`, which can be found [here](https://www.haskell.org/platform/).

To build, run `cabal new-build` in the root of the repository.

You can then either install the package globally running `cabal install`, or run the local build with `cabal new-run` (e.g. `cabal new-run . -- run --path Samples/Code.tl`).

Usage (assuming you `cabal install`ed the project):

```sh
tli run --path <path> # Runs the program and displays the final output.
tli run --small-step --path <path> # Runs and displays every intermediate program state.

tli dump-ast --path <path> # Parses, type-checks, then prints the AST of the program.
tli dump-ast --no-type-check --path <path> # Prints the AST straight from the parser.
```

## License

This repository is licensed under the terms of the MIT License.
For more details, see [the license file](LICENSE.txt).
