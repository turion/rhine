attrs:

let
  settings = import ./defaults.nix // attrs;
  haskellPackages = settings.pkgs.haskell.packages.${settings.compiler};
in
haskellPackages.override {
  overrides = super: self: {
    rhine = self.callCabal2nix "rhine" ../rhine {};
    rhine-examples = self.callCabal2nix "rhine-examples" ../rhine-examples {};
    rhine-gloss = self.callCabal2nix "rhine-gloss" ../rhine-gloss {};
  };
}
