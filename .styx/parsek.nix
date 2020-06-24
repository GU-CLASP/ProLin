{ mkDerivation, base, fetchgit, stdenv }:
mkDerivation {
  pname = "parsek";
  version = "1.0.3.0";
  src = fetchgit {
    url = "https://github.com/jyp/Parsek.git";
    sha256 = "1gi76ndridjgdm5swzfw164kipb1mlxsriq5ms569srlnhrdvn2n";
    rev = "cb92f2cfc5dde7b4ebc6c9e0de66455ee269281d";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base ];
  description = "Parallel Parsing Processes";
  license = stdenv.lib.licenses.gpl3;
}
