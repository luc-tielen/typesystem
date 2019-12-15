
# Typesystem

This repo contains some experiments using a bidirectional typechecker.
The approach is heavily based upon [this tutorial](http://www.davidchristiansen.dk/tutorials/bidirectional.pdf)
and the purescript typechecker and does the following things:

- Step by step error reporting
- Support for typed holes
- Returns a typed expression, with type information at every node of the tree

The [type-variables branch](https://github.com/luc-tielen/typesystem/tree/type-variables)
contains some additional code that allows users to specify types containing
type variables (unification system based on the Purescript typechecker).


## Running the examples

Right now the typechecker and examples are all located in a single file
([src/Main.hs](https://github.com/luc-tielen/typesystem/blob/master/src/Main.hs)).

Execute the following commands to see the results:

```bash
$ git clone git@github.com:luc-tielen/typesystem.git
$ nix-shell   # optional, if you prefer using Nix
$ cabal new-configure
$ cabal new-run typesystem
```

Output for the examples can also be found [here](https://github.com/luc-tielen/typesystem/blob/master/output.txt).
