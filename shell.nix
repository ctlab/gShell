with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, bytestring, cereal, containers, data-default
             , deepseq, directory, directory-tree, either, filepath, lens
             , process, random, regex-posix, stdenv, time, transformers, unix
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
                 directory-tree either filepath lens process random regex-posix time
                 transformers unix Unixutils
               ];
               homepage = "https://github.com/ctlab/gShell";
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
