{ nixpkgs ? import <nixpkgs> {} }:

let
  travisJobGHCEnvs = (builtins.fromJSON (builtins.readFile ../.travis.yml)).ghc;
  getGHCVersionFromEnv = env: builtins.replaceStrings ["."] [] env;
  supportedGHCVersions = builtins.map getGHCVersionFromEnv travisJobGHCEnvs;
  buildRhineWith = version: import ../default.nix { compiler = version; };
in builtins.map buildRhineWith supportedGHCVersions
