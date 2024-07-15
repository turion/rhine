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

      # The names of all Haskell packages in this repository, defined as all the directories with *.cabal files in them.
      pnames = map (path: baseNameOf (dirOf path)) (lib.fileset.toList (lib.fileset.fileFilter (file: file.hasExt "cabal") ./.));

      # All GHC versions that this project is tested with.
      # To be kept in sync with the `tested-with:` section in rhine.cabal.
      # To do: Automated check whether this is the same as what get-tested returns.
      # Currently blocked on https://github.com/Kleidukos/get-tested/issues/39
      supportedGhcs = [ "ghc92" "ghc94" "ghc96" "ghc98" ];

      # All Haskell packages defined here that contain a library section
      libPnames = filter (pname: pname != "rhine-examples") pnames;

      # The Haskell packages set, for every supported GHC version
      hpsFor = pkgs:
        lib.filterAttrs (name: _: elem name supportedGhcs) pkgs.haskell.packages
        // { default = pkgs.haskellPackages; };

      # A haskellPackages overlay containing everything defined in this repo
      rhinePackages = hfinal: hprev:
        lib.genAttrs pnames (pname: hfinal.callCabal2nix pname ./${pname} { });

      # A nixpkgs overlay containing everything defined in this repo, for reuse in downstream projects
      overlay = final: prev:
        let
          hps = hpsFor final;

          # Overrides that are necessary because of dependencies not being up to date or fixed yet in nixpkgs.
          # Check on nixpkgs bumps whether some of these can be removed.
          temporaryHaskellOverrides = with prev.haskell.lib.compose; [
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

        in
        {
          # The Haskell package set containing the packages defined in this repo
          haskell = prev.haskell // {
            packageOverrides = with prev.haskell.lib.compose; lib.composeManyExtensions ([
              prev.haskell.packageOverrides
              rhinePackages
            ] ++ temporaryHaskellOverrides);
          };

          # Helper packages containing aspects of the whole rhine build:
          # All executables, built with the nixpkgs-default GHC
          rhine-bin = prev.buildEnv
            {
              name = "rhine-bin";
              paths = map (pname: hps.default.${pname}) pnames;
              pathsToLink = [ "/bin" ];
            };
          # All libraries for all GHC versions
          rhine-lib = prev.buildEnv
            {
              name = "rhine-lib";
              paths = lib.mapCartesianProduct
                ({ hp, pname }: hp.${pname})
                { hp = attrValues hps; pname = pnames; };
              pathsToLink = [ "/lib" ];
            };
          # All haddocks
          rhine-docs = prev.buildEnv
            {
              name = "rhine-docs";
              paths = map (pname: prev.haskell.lib.documentationTarball hps.default.${pname}) libPnames;
            };
          # All sdist tarballs for Hackage publication
          rhine-sdist = prev.buildEnv
            {
              name = "rhine-sdist";
              paths = map (pname: prev.haskell.lib.sdistTarball hps.default.${pname}) libPnames;
            };

          # All rhine build products
          rhine-all = prev.symlinkJoin
            {
              name = "rhine-all";
              paths = with final; [ rhine-bin rhine-lib ];
              postBuild = ''
                ln -s ${final.rhine-docs} $out/docs
                ln -s ${final.rhine-sdist} $out/sdist
              '';
            };
        };

      # Helper to build a flake output for all systems that are defined in nixpkgs
      forAllPlatforms = f:
        mapAttrs (system: pkgs: f system (pkgs.extend overlay)) inputs.nixpkgs.legacyPackages;
    in
    {
      # Reexport the overlay so other downstream flakes can use it to develop rhine projects with low effort.
      overlays.default = overlay;

      # Usage: nix fmt
      formatter = forAllPlatforms (system: pkgs: pkgs.nixpkgs-fmt);

      # Usage: nix build # This builds all rhine packages on all GHCs
      packages = forAllPlatforms (system: pkgs: {
        default = pkgs.rhine-all;
      });

      legacyPackages = forAllPlatforms (system: pkgs: {
        inherit (pkgs) haskell haskellPackages rhine-all;
      });

      # Usage: nix develop .#ghc98
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
