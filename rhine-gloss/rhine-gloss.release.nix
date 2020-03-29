let
  config = {
    packageOverrides = nixpkgs: rec {
      haskellPackages = nixpkgs.haskellPackages.override {
        overrides = hnew: hold: rec {
          rhine = hnew.callPackage ../rhine/rhine.nix {};
          dunai = hnew.callPackage ../rhine/dunai.nix {};
          simple-affine-space = hnew.callPackage ../rhine/simple-affine-space.nix {};
          rhine-gloss = hnew.callPackage ./rhine-gloss.nix {};
        };
      };
    };
  };
  nixpkgs = import <nixpkgs> { inherit config; };
in
{
  rhine-gloss = nixpkgs.haskellPackages.rhine-gloss;
}
