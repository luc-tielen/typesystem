
# Typesystem

This repo contains some experiments using a bidirectional typechecker.
The approach is mostly based upon [this tutorial](http://www.davidchristiansen.dk/tutorials/bidirectional.pdf),
but adds some extra things:

- Step by step error reporting
- Support for typed holes


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
