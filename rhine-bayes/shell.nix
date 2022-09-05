# Use this file to install the `monad-bayes` dependency on newer GHCs
# (See https://github.com/NixOS/nixpkgs/pull/189234)
let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages;
#   haskellPackages = haskell.packages.ghc884;
  pkgsSelector = packages: with packages; [
    adjunctions
    cabal-install
    dunai
    finite-typelits
    gloss
    MonadRandom
    OpenGL
    vector-sized
    (with pkgs.haskell.lib; markUnbroken (doJailbreak monad-bayes))
  ];
  ghc = haskellPackages.ghcWithPackages pkgsSelector;
in

pkgs.mkShell {
    # nativeBuildInputs is usually what you want -- tools you need to run
    nativeBuildInputs = [
      ghc
    ];
}
