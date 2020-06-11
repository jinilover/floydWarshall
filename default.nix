{ mkDerivation, attoparsec, base-noprelude, classy-prelude
, containers, hedgehog, lens, mtl, protolude, split, stdenv, tasty
, tasty-hedgehog, tasty-hunit, text, time, vector, writer-cps-mtl
}:
mkDerivation {
  pname = "floydWarshall";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base-noprelude classy-prelude containers lens mtl
    protolude split text time vector writer-cps-mtl
  ];
  executableHaskellDepends = [
    attoparsec base-noprelude classy-prelude containers lens mtl
    protolude split text time vector writer-cps-mtl
  ];
  testHaskellDepends = [
    attoparsec base-noprelude classy-prelude containers hedgehog lens
    mtl protolude split tasty tasty-hedgehog tasty-hunit text time
    vector writer-cps-mtl
  ];
  description = "Apply FloydWarshall algorithm";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
