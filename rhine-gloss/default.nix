let
  config = {
    allowBroken = true;
    packageOverrides = nixpkgs: rec {
      haskellPackages = nixpkgs.haskellPackages.override {
        overrides = self: super: rec {
          rhine = self.callCabal2nix "rhine" ../rhine {};
          simple-affine-space = self.callHackage "simple-affine-space" "0.1" {
            mkDerivation = args: self.mkDerivation(args // { doCheck = false; });
          };
          rhine-gloss = self.callCabal2nix "rhine-gloss" ./. {};
        };
      };
    };
  };
  nixpkgs = import <nixpkgs> { inherit config; };
in
{
  rhine-gloss = nixpkgs.haskellPackages.rhine-gloss;
}
