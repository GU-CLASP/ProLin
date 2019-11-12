{ mkDerivation, aeson, base, base-compat, bytestring, containers
, criterion, deepseq, fetchgit, pretty, stdenv, text
, unordered-containers, wl-pprint
}:
mkDerivation {
  pname = "pretty-compact";
  version = "3.0";
  src = fetchgit {
    url = "https://github.com/jyp/prettiest.git";
    sha256 = "04982rx0riv90nr80gicp80hvj1pf7zlmswicv13dhz6pphdsl72";
    rev = "3ef1954f045b026c9842cac51549203370088120";
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
