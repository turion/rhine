{
  description = "rhine";
  nixConfig.bash-prompt = "\[rhine\]$ ";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";
    flake-utils.url = "github:numtide/flake-utils";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    haskell-flake-utils = {
      url = "github:ivanovs-4/haskell-flake-utils";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

outputs = { self, nixpkgs, flake-utils, haskell-flake-utils, flake-compat, ... }:
  haskell-flake-utils.lib.simpleCabalProject2flake {
    inherit self nixpkgs;

    systems = [
      # Tested in CI
      "x86_64-linux"
      "x86_64-darwin"
      # Tested by maintainers
      "aarch64-darwin"
      # Not sure we can test this in CI
      "i686-linux"
    ];

    hpPreOverrides = { pkgs, ... }: self: super:
      with pkgs.haskell.lib;
      with haskell-flake-utils.lib;
      tunePackages pkgs super {
        monad-bayes = [ (jailbreakUnbreak pkgs) dontCheck ];
      };

    name = "rhine";
    packageNames = [ "rhine-gloss" "rhine-terminal" "rhine-examples" "rhine-bayes" ];
  };
}
