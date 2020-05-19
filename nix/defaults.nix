# Sets default versions for global variables
rec {
  compiler = "ghc865";
  nixpkgs = import <nixpkgs> {};
  inherit (nixpkgs) pkgs lib;
}
