{ mkDerivation, base-noprelude, classy-prelude, containers, hspec
, HUnit, parsec, protolude, split, stdenv, time, vector
, writer-cps-mtl
}:
mkDerivation {
  pname = "fsm-rwst-parsec-floydWarshall";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base-noprelude classy-prelude containers parsec protolude split
    time vector writer-cps-mtl
  ];
  executableHaskellDepends = [
    base-noprelude classy-prelude containers parsec protolude split
    time vector writer-cps-mtl
  ];
  testHaskellDepends = [
    base-noprelude classy-prelude containers hspec HUnit parsec
    protolude split time vector writer-cps-mtl
  ];
  description = "Apply FloydWarshall algorithm";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
