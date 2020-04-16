let
  config = {
    allowBroken = true;
    packageOverrides = nixpkgs: rec {
      haskellPackages = nixpkgs.haskellPackages.override {
        overrides = self: super: rec {
          simple-affine-space = self.callHackage "simple-affine-space" "0.1" {
            mkDerivation = args: self.mkDerivation(args // { doCheck = false; });
          };
          rhine = self.callCabal2nix "rhine" ./. {};
        };
      };
    };
  };
  nixpkgs = import <nixpkgs> { inherit config; };
in
{
  rhine = nixpkgs.haskellPackages.rhine;
}
