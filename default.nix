{ mkDerivation, base-noprelude, classy-prelude, containers
, hedgehog, lens, mtl, parsec, protolude, split, stdenv, tasty
, tasty-hedgehog, tasty-hunit, text, time, vector, writer-cps-mtl
}:
mkDerivation {
  pname = "fsm-rwst-parsec-floydWarshall";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base-noprelude classy-prelude containers lens mtl parsec protolude
    split text time vector writer-cps-mtl
  ];
  executableHaskellDepends = [
    base-noprelude classy-prelude containers lens mtl parsec protolude
    split text time vector writer-cps-mtl
  ];
  testHaskellDepends = [
    base-noprelude classy-prelude containers hedgehog lens mtl parsec
    protolude split tasty tasty-hedgehog tasty-hunit text time vector
    writer-cps-mtl
  ];
  description = "Apply FloydWarshall algorithm";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
