{
  description = "rhine";
  nixConfig.bash-prompt = "\[rhine\]$ ";

  nixConfig = {
    extra-substituters = [
      "https://rhine.cachix.org"
    ];
    extra-trusted-public-keys = [
      "rhine.cachix.org-1:oFsONI6lXn3XG4aVmIURDa2Rn0dW5XTPy6eJWROIs8k="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";
  };

  outputs = inputs:
    with builtins;
    let
      lib = inputs.nixpkgs.lib;
      pnames = [
        "automaton"
        "rhine"
        "rhine-bayes"
        "rhine-examples"
        "rhine-gloss"
        "rhine-terminal"
      ];
      supportedGhcs = [ "ghc92" "ghc94" "ghc96" "ghc98" ];
      libPnames = filter (pname: pname != "rhine-examples") pnames;
      hpsFor = pkgs:
        lib.filterAttrs (name: _: elem name supportedGhcs) pkgs.haskell.packages
        // { default = pkgs.haskellPackages; };
      rhinePackages = hfinal: hprev:
        lib.genAttrs pnames (pname: hfinal.callCabal2nix pname ./${pname} { });
      overlay = final: prev:
        let hps = hpsFor final; in {
          haskell = prev.haskell // {
            packageOverrides = with prev.haskell.lib.compose; lib.composeManyExtensions [
              prev.haskell.packageOverrides
              rhinePackages
              (hfinal: hprev: {
                monad-bayes = markUnbroken hprev.monad-bayes;
                monad-schedule = hprev.callHackageDirect
                  {
                    pkg = "monad-schedule";
                    ver = "0.2";
                    sha256 = "sha256-Z9lAxkvJDH9aQZd65bGOQI3EGH7oSAhK0nuBKULgiCE=";
                  }
                  { };
                time-domain = hprev.callHackageDirect
                  {
                    pkg = "time-domain";
                    ver = "0.1.0.5";
                    sha256 = "sha256-llDBQuU5ez/0MiOIMH97P4BQhFDyPfTMWinq1wJrDGI=";
                  }
                  { };
              })
              (hfinal: hprev: lib.optionalAttrs prev.stdenv.isDarwin {
                monad-schedule = dontCheck hprev.monad-schedule;
              })
              (hfinal: hprev: lib.optionalAttrs (lib.versionOlder hprev.ghc.version "9.4") {
                time-domain = doJailbreak hprev.time-domain;
              })
            ];
          };
          rhine-bin = prev.buildEnv {
            name = "rhine-bin";
            paths = map (pname: hps.default.${pname}) pnames;
            pathsToLink = [ "/bin" ];
          };
          rhine-lib = prev.buildEnv {
            name = "rhine-lib";
            paths = lib.mapCartesianProduct
              ({ hp, pname }: hp.${pname})
              { hp = attrValues hps; pname = pnames; };
            pathsToLink = [ "/lib" ];
          };
          rhine-docs = prev.buildEnv {
            name = "rhine-docs";
            paths = map (pname: prev.haskell.lib.documentationTarball hps.default.${pname}) libPnames;
          };
          rhine-sdist = prev.buildEnv {
            name = "rhine-sdist";
            paths = map (pname: prev.haskell.lib.sdistTarball hps.default.${pname}) libPnames;
          };
          rhine-all = prev.symlinkJoin {
            name = "rhine-all";
            paths = with final; [ rhine-bin rhine-lib ];
            postBuild = ''
              ln -s ${final.rhine-docs} $out/docs
              ln -s ${final.rhine-sdist} $out/sdist
            '';
          };
        };
      forAllPlatforms = f:
        mapAttrs (system: pkgs: f system (pkgs.extend overlay)) inputs.nixpkgs.legacyPackages;
    in
    {
      overlays.default = overlay;

      formatter = forAllPlatforms (system: pkgs: pkgs.nixpkgs-fmt);

      packages = forAllPlatforms (system: pkgs: {
        default = pkgs.rhine-all;
      });

      legacyPackages = forAllPlatforms (system: pkgs: {
        inherit (pkgs) haskell haskellPackages rhine-all;
      });

      devShells = forAllPlatforms (systems: pkgs: mapAttrs
        (_: hp: hp.shellFor {
          packages = ps: map (pname: ps.${pname}) pnames;
          nativeBuildInputs = with hp; [
            cabal-install
            fourmolu
            haskell-language-server
          ];
        })
        (hpsFor pkgs));
    };
}
