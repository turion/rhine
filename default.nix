attrs@{ ... }:

let
  localPackages = import ./nix/localPackages.nix attrs;
in
{
  inherit (localPackages)
    rhine
    rhine-gloss
    rhine-examples
  ;
}
