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

    terminal-fork = {
      url = "github:jmatsushita/haskell-terminal/reader";
      flake = false;
  };

  };

outputs = { self, nixpkgs, flake-utils, haskell-flake-utils, flake-compat, terminal-fork, ... }:
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
    hpPreOverrides = { pkgs }: new: old:
      with pkgs.haskell.lib; {
        terminal = old.callCabal2nix "terminal" terminal-fork {};
      };

  };
}
