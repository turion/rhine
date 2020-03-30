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
          rhine-examples = self.callCabal2nix "rhine-examples" ./. {};
        };
      };
    };
  };
  nixpkgs = import <nixpkgs> { inherit config; };
in
{
  rhine-examples = nixpkgs.haskellPackages.rhine-examples;
}
