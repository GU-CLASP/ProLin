{ nixpkgs ? import <nixpkgs> {} }:
let nixpkgs_source =
    fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-23.11.tar.gz";
    nixpkgs' = (import nixpkgs_source){};
    pkgs = nixpkgs'.pkgs;
    hp = pkgs.haskellPackages.override {
        overrides = self: super:
        let callPackage = self.callPackage; in {
              pretty-compact = callPackage ./.styx/pretty-compact.nix {};
            };
    };
in hp.shellFor {
  packages = hpkgs: [
    # reuse the nixpkgs for this package
    # hpkgs.distribution-nixpkgs
    # call our generated Nix expression manually
    (hpkgs.callPackage ./default.nix { })
  ];
}
