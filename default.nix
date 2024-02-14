{ mkDerivation, alex, array, base, BNFC, Cabal, containers
, directory, filepath, happy, haskeline, lib, mtl, network
, network-simple, optparse-applicative, parsek, pretty-compact
, process, transformers
}:
mkDerivation {
  pname = "prolin";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal directory process ];
  executableHaskellDepends = [
    array base BNFC containers directory filepath haskeline mtl network
    network-simple optparse-applicative parsek pretty-compact
    transformers
  ];
  executableToolDepends = [ alex happy ];
  homepage = "https://github.com/GU-CLASP/prolin";
  description = "Implementation of a type";
  license = lib.licenses.gpl3Only;
  mainProgram = "pli";
}
