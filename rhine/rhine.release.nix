let
  config = {
    packageOverrides = nixpkgs: rec {
      haskellPackages = nixpkgs.haskellPackages.override {
        overrides = hnew: hold: rec {
          rhine = hnew.callPackage ./rhine.nix {};
          dunai = hnew.callPackage ./dunai.nix {};
          simple-affine-space = hnew.callPackage ./simple-affine-space.nix {};
        };
      };
    };
  };
  nixpkgs = import <nixpkgs> { inherit config; };
in
{
  rhine = nixpkgs.haskellPackages.rhine;
}
