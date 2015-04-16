{ mkDerivation, base, bytestring, cereal, containers, data-default
, directory, directory-tree, filepath, lens, process, random
, stdenv, time, transformers, unix, either, Unixutils
}:
mkDerivation {
  pname = "gShell";
  version = "0.0.0.0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base bytestring cereal containers data-default directory
    directory-tree filepath lens process random time transformers unix
    either Unixutils
  ];
  homepage = "https://github.com/ctlab/gShell";
  license = stdenv.lib.licenses.mit;
}
