{ cabal, process, daemons, dataDefault, cereal, lens }:  

cabal.mkDerivation (self: {
  pname = "gShell";
  version = "0.0.0.0.0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    process daemons dataDefault cereal lens
  ];
})
