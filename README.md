[![Build Status](https://travis-ci.org/ctlab/gShell.svg)](https://travis-ci.org/ctlab/gShell)
### Build
* Using [nix](https://nixos.org/nix/):
```
nix-build
```
* Using [cabal](https://www.haskell.org/cabal/):
```
cabal sandbox init
cabal install --only-dependencies
cabal build
```
