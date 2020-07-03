{ mkDerivation, aeson, base, base-compat, bytestring, containers
, criterion, deepseq, fetchgit, pretty, stdenv, text
, unordered-containers, wl-pprint
}:
mkDerivation {
  pname = "pretty-compact";
  version = "3.0";
  src = fetchgit {
    url = "https://github.com/jyp/prettiest.git";
    sha256 = "04lwkh21m7kjrf3bsra29a11qhyhs124h9w692xlzzbzdz4g1sws";
    rev = "0fb7f1b2209d6763b9467c5aae2f952f8a929aed";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base base-compat containers ];
  benchmarkHaskellDepends = [
    aeson base base-compat bytestring criterion deepseq pretty text
    unordered-containers wl-pprint
  ];
  description = "Pretty-printing library";
  license = "GPL";
}
