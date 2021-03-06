{ nixpkgs ? import <nixpkgs> {}

 }:
let nixpkgs_source =
fetchTarball "https://github.com/NixOS/nixpkgs-channels/archive/55668eb671b915b49bcaaeec4518cc49d8de0a99.tar.gz";
  nixpkgs' = (import nixpkgs_source){};
in with nixpkgs'.pkgs;
let hp = haskellPackages.override{
    overrides = self: super: {
      parsek = self.callPackage ./parsek.nix {};
      pretty-compact = self.callPackage ./pretty-compact.nix {};
      prolin = self.callPackage ./prolin.nix {};
      };};
     getHaskellDeps = ps: path:
        let f = import path;
            gatherDeps = { buildDepends ? [], libraryHaskellDepends ? [], executableHaskellDepends ? [], libraryToolDepends ? [], executableToolDepends ? [], ...}:
               buildDepends ++ libraryHaskellDepends ++ executableHaskellDepends ++ libraryToolDepends ++ executableToolDepends;
            x = f (builtins.intersectAttrs (builtins.functionArgs f)
                                               (ps // 
                                                nixpkgs'.pkgs) # can also depend on non-haskell packages
                   // {stdenv = stdenv; mkDerivation = gatherDeps;});
        in x;
ghc = hp.ghcWithPackages (ps: with ps; stdenv.lib.lists.subtractLists
[prolin]
([ cabal-install 
parsek pretty-compact BNFC pretty-compact
  ]  ++ getHaskellDeps ps ./prolin.nix));
in
pkgs.stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [ ghc ];
  shellHook = ''
 export LANG=en_US.UTF-8
 eval $(egrep ^export ${ghc}/bin/ghc)
'';
}
