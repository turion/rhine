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
      # Contains e.g.: rhine, rhine-examples, rhine-bayes, ...
      pnames = map (path: baseNameOf (dirOf path)) (lib.fileset.toList (lib.fileset.fileFilter (file: file.hasExt "cabal") ./.));

      # All GHC versions that this project is tested with.
      # To be kept in sync with the `tested-with:` section in rhine.cabal.
      # To do: Automated check whether this is the same as what get-tested returns.
      # Currently blocked on https://github.com/Kleidukos/get-tested/issues/39
      supportedGhcs = [
        "ghc94"
        "ghc96"
        "ghc98"
        "ghc910"
      ];

      # All Haskell packages defined here that contain a library section
      libPnames = filter (pname: pname != "rhine-examples") pnames;

      # The Haskell packages set, for every supported GHC version
      hpsFor = pkgs:
        lib.genAttrs supportedGhcs (ghc: pkgs.haskell.packages.${ghc})
        // { default = pkgs.haskellPackages; };

      # A nixpkgs overlay containing necessary overrides on dependencies added in rhine
      localDependenciesOverlay = final: prev:
        let
          # Overrides that are necessary because of dependencies not being up to date or fixed yet in nixpkgs.
          # Check on nixpkgs bumps whether some of these can be removed.
          temporaryHaskellOverrides = with prev.haskell.lib.compose; [
            (hfinal: hprev: {
              monad-bayes = markUnbroken hprev.monad-bayes;
              changeset = markUnbroken (doJailbreak hprev.changeset);
            })
            (hfinal: hprev: lib.optionalAttrs prev.stdenv.isDarwin {
              monad-schedule = dontCheck hprev.monad-schedule;
            })
            (hfinal: hprev: lib.optionalAttrs (lib.versionAtLeast hprev.ghc.version "9.10") {
              # Remove these after https://github.com/turion/rhine/issues/399
              gloss-rendering = doJailbreak hprev.gloss-rendering;
              gloss = doJailbreak hprev.gloss;
            })
          ];
        in
        {
          # The Haskell package set containing the packages defined in this repo
          haskell = prev.haskell // {
            packageOverrides = lib.composeManyExtensions ([
              prev.haskell.packageOverrides
            ] ++ temporaryHaskellOverrides);
          };
        };

      # A nixpkgs overlay containing necessary overrides on all dependencies, for reuse in downstream projects
      dependenciesOverlay = lib.composeManyExtensions [
        localDependenciesOverlay
      ];

      # A haskellPackages overlay containing everything defined in this repo
      rhinePackagesOverlay = hfinal: hprev:
        lib.genAttrs pnames (pname: hfinal.callCabal2nix pname ./${pname} { });

      # A nixpkgs overlay containing everything defined in this repo
      localOverlay = final: prev:
        let
          hps = hpsFor final;
        in
        {
          # The Haskell package set containing the packages defined in this repo
          haskell = prev.haskell // {
            packageOverrides = lib.composeManyExtensions [
              prev.haskell.packageOverrides
              rhinePackagesOverlay
            ];
          };

          # Helper packages containing aspects of the whole rhine build:
          # All executables, built with the nixpkgs-default GHC
          # We build these only with one GHC because otherwise the bin names would clash
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

          # Haddocks for all packages that can be uploaded to Hackage
          rhine-docs = prev.buildEnv
            {
              name = "rhine-docs";
              paths = map (pname: prev.haskell.lib.documentationTarball hps.default.${pname}) libPnames;
            };

          # Sdist tarballs for all packages that can be uploaded to Hackage
          rhine-sdist = prev.buildEnv
            {
              name = "rhine-sdist";
              paths = map (pname: prev.haskell.lib.sdistTarball hps.default.${pname}) libPnames;
            };

          # All rhine build products
          rhine-all = prev.symlinkJoin
            {
              name = "rhine-all";
              paths = with final; [
                rhine-bin
                rhine-lib
                (prev.linkFarm "docsAndSdist" { docs = final.rhine-docs; sdist = rhine-sdist; })
              ];
            };
        };

      overlay = lib.composeManyExtensions
        [
          dependenciesOverlay
          localOverlay
        ];

      # Helper to build a flake output for all systems that are defined in nixpkgs
      forAllPlatforms = f:
        mapAttrs (system: pkgs: f system (pkgs.extend overlay)) inputs.nixpkgs.legacyPackages;
    in
    {
      # Reexport the overlay so other downstream flakes can use it to develop rhine projects with low effort.
      overlays = {
        inherit dependenciesOverlay localOverlay;
        default = overlay;
      };

      # Usage: nix fmt
      formatter = forAllPlatforms (system: pkgs: pkgs.nixpkgs-fmt);

      # This builds all rhine packages on all GHCs, as well as docs and sdist
      # Usage: nix build
      packages = forAllPlatforms (system: pkgs: {
        default = pkgs.rhine-all;
      } // lib.mapAttrs (ghcVersion: haskellPackages: pkgs.linkFarm "rhine-all-${ghcVersion}" (lib.genAttrs pnames (pname: haskellPackages.${pname}))) (hpsFor pkgs));

      # We re-export the entire nixpkgs package set with our overlay.
      # Usage examples:
      # - nix build .#haskellPackages.rhine
      # - nix build .#haskell.packages.ghc98.rhine
      # - nix build .#rhine-sdist
      legacyPackages = forAllPlatforms (system: pkgs: pkgs);

      # Usage: nix develop (will use the default GHC)
      # Alternatively, specify the GHC: nix develop .#ghc98
      devShells = forAllPlatforms (systems: pkgs: mapAttrs
        (_: hp: hp.shellFor {
          packages = ps: map (pname: ps.${pname}) pnames;
          nativeBuildInputs = (with hp; lib.optional (lib.versionAtLeast hp.ghc.version "9.4")
            haskell-language-server
          ) ++ (with pkgs; [
            haskellPackages.fourmolu
            haskellPackages.cabal-gild
            cabal-install
          ]);
        })
        (hpsFor pkgs));

      # Doesn't build on darwin
      # https://github.com/NixOS/nixpkgs/issues/367686
      supportedGhcs = lib.lists.removePrefix [ "ghc94" ] supportedGhcs;
    };
}
