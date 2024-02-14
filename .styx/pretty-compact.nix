{ mkDerivation, aeson, base, base-compat, bytestring, containers
, criterion, deepseq, fetchgit, lib, pretty, text
, unordered-containers, wl-pprint
}:
mkDerivation {
  pname = "pretty-compact";
  version = "3.1";
  src = fetchgit {
    url = "https://github.com/jyp/prettiest.git";
    sha256 = "1ix9hpvqpvhsmr1l6lygwhb11izwndmpsm1sgjigvmc8y86m6jny";
    rev = "71fe5b4a88154b9d70e7053a330ad29f03b51d94";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base base-compat containers ];
  benchmarkHaskellDepends = [
    aeson base base-compat bytestring criterion deepseq pretty text
    unordered-containers wl-pprint
  ];
  description = "Pretty-printing library";
  license = lib.licenses.lgpl3Only;
}
