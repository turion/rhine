{
  description = "rhine";
  nixConfig.bash-prompt = "\[rhine\]$ ";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    haskell-flake-utils = {
      url = "github:ivanovs-4/haskell-flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    terminal-src = {
      url = "github:jmatsushita/haskell-terminal/reader";
      flake = false;
    };

  };

  outputs = { self, nixpkgs, flake-utils, haskell-flake-utils, flake-compat, terminal-src, ... }:
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

      name = "rhine";
      packageNames = [ "rhine-gloss" "rhine-terminal" "rhine-examples" ];

      shellExtBuildInputs = {pkgs}: with pkgs; [
        haskellPackages.haskell-language-server
      ];

      hpPreOverrides = { pkgs }: new: old:
        with pkgs.haskell.lib; with haskell-flake-utils.lib;
        {
          monad-schedule = old.callHackageDirect { pkg = "monad-schedule"; ver = "0.1.2.0"; sha256 = "sha256-7zXI37brdrqlgiRo/RseP0iMhBferzDM+Qiu5F1hhus="; } {};
          terminal = old.callCabal2nix "terminal" "${terminal-src}" {};
        };

    };
}
