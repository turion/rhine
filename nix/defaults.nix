# Sets default versions for global variables
rec {
  compiler = "ghc884";
  nixpkgs = import <nixpkgs> {};
  inherit (nixpkgs) pkgs lib;
}
