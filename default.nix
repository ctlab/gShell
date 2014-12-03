{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callPackage ./gShell.nix {}
