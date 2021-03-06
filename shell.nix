{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, cereal, containers
      , data-default, deepseq, directory, directory-tree, either, fgl
      , filepath, graphviz, lens, optparse-applicative, process, random
      , regex-posix, split, stdenv, text, time, transformers, unix
      , Unixutils
      }:
      mkDerivation {
        pname = "gShell";
        version = "0.0.0.0.0.1";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [
          base bytestring cereal containers data-default deepseq directory
          directory-tree either fgl filepath graphviz lens
          optparse-applicative process random regex-posix split text time
          transformers unix Unixutils
        ];
        homepage = "https://github.com/ctlab/gShell";
        license = stdenv.lib.licenses.mit;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
