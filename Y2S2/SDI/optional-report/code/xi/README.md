# Xi

Xi is a small functional programming language with structural subtyping, with a tree-walking interpreter implemented in Haskell.

## Building & running

To build the project, you will need `cabal`, the Haskell build system, which can be downloaded via [ghcup](https://www.haskell.org/ghcup/) if you're using Linux or Mac, or via [the Haskell Platform](https://www.haskell.org/platform/) if you're using Windows.

To build the project, run the following command, in the `xi` folder:

```sh
cd code/xi # Make sure this is your current working directory
cabal new-build
```

You then have two options for running the project:

### Option 1

Run the locally-built executable, using the following command:

```sh
cabal new-run . <file>
```

Where `<file>` is the Xi source file you want to execute.

### Option 2

Install the project, by running:

```sh
cabal install
```

This will copy the executable to `cabal`'s `bin` directory (which should have been added to your `PATH` during installation).

Doing this will allow you to easily invoke the interpreter by running:

```sh
xi <path>
```

It will also allow you to easily use the interpreter in shebangs (as can also be seen in the [examples](Examples)):

```xi
#!/usr/bin/env xi

print "Hello World from Xi!"
```

## VSCode Extension

The repository also includes a VSCode extension that adds syntax highlighting for Xi source files.

If you're using Linux and you have a default installation of VSCode, you can easily install the extension by running:

```sh
./install-vscode-ext.sh
```
