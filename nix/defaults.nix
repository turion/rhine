# Sets default versions for global variables
rec {
  compiler = "ghc8101";
  nixpkgs = import <nixpkgs> {};
  inherit (nixpkgs) pkgs lib;
}
