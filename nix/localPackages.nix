attrs:

let
  settings = import ./defaults.nix // attrs;
  haskellPackages = settings.pkgs.haskell.packages.${settings.compiler};
  inherit (settings.pkgs) fetchFromGitHub;
  essence-of-live-coding-src = fetchFromGitHub {
    owner = "turion";
    repo = "essence-of-live-coding";
    rev = "67fa15029028b29c89599c0cdbf64294ab96f291";
    sha256 = "0q2901w1gh3y39ws2sm7h7sqbbk2j7kasy4k5xd0mk670bsd4i5c";
  };
  dunai-live-src = fetchFromGitHub {
    owner = "turion";
    repo = "dunai-live";
    rev = "712e0c21b7b44d2a6d854b280b9fbc34a77a9202";
    sha256 = "14rsc39jnawv4r6h51kyyan6qx59nwzw0a32xj23ysrv3g59wy7g";
  };
in
haskellPackages.override {
  overrides = super: self: {
    rhine = self.callCabal2nix "rhine" ../rhine {};
    rhine-examples = self.callCabal2nix "rhine-examples" ../rhine-examples {};
    rhine-gloss = self.callCabal2nix "rhine-gloss" ../rhine-gloss {};
    # essence-of-live-coding = import essence-of-live-coding-src {}; # FIXME Remove when this is stably on hackage & stackage
    essence-of-live-coding = self.callCabal2nix "essence-of-live-coding" "${essence-of-live-coding-src}/essence-of-live-coding" {}; # FIXME Remove when this is stably on hackage & stackage
    dunai-live = self.callCabal2nix "dunai-live" dunai-live-src {}; # FIXME Remove when this is stably on hackage & stackage
  };
}
