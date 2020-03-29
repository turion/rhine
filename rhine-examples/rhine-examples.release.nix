let
  config = {
    packageOverrides = nixpkgs: rec {
      haskellPackages = nixpkgs.haskellPackages.override {
        overrides = hnew: hold: rec {
          rhine = hnew.callPackage ../rhine/rhine.nix {};
          dunai = hnew.callPackage ../rhine/dunai.nix {};
          simple-affine-space = hnew.callPackage ../rhine/simple-affine-space.nix {};
          rhine-examples = hnew.callPackage ./rhine-examples.nix {};
        };
      };
    };
  };
  nixpkgs = import <nixpkgs> { inherit config; };
in
{
  rhine-examples = nixpkgs.haskellPackages.rhine-examples;
}
