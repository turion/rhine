attrs@{ ... }:

drv:

let
  settings = import ./defaults.nix // attrs;
  localPackages = import ./localPackages.nix attrs;
in
drv.env.overrideAttrs (old:
  with localPackages; {
    nativeBuildInputs = old.nativeBuildInputs ++ [
      # brittany # Y u not work
      ghcide
      hlint
      (settings.pkgs.haskell.lib.dontCheck implicit-hie)
    ];
  }
)
