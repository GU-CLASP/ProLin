{ mkDerivation, alex, array, base, BNFC, Cabal, containers
, directory, filepath, happy, haskeline, mtl, network
, network-simple, parsek, pretty-compact, process, stdenv
, transformers
}:
mkDerivation {
  pname = "prolin";
  version = "0.1.0";
  src = /home/jyp/repo/gu/ProLin;
  isLibrary = false;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal directory process ];
  executableHaskellDepends = [
    array base BNFC containers directory filepath haskeline mtl network
    network-simple parsek pretty-compact transformers
  ];
  executableToolDepends = [ alex happy ];
  homepage = "https://github.com/GU-CLASP/prolin";
  description = "Implementation of a type";
  license = stdenv.lib.licenses.gpl3;
}
