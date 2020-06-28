let
  mypkgs = import ./. {};
  shellFrom = import ./nix/shellFrom.nix {};
in shellFrom mypkgs.rhine-gloss
