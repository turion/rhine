{
  description = "rhine — functional reactive programming with type-level clocks";
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
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
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
        "ghc912"
      ];

      # All Haskell packages defined here that contain a library section
      libPnames = filter (pname: pname != "rhine-examples") pnames;

      # The Haskell packages set, for every supported GHC version
      hpsFor = pkgs:
        lib.genAttrs supportedGhcs (ghc: pkgs.haskell.packages.${ghc})
        // { default = pkgs.haskell.packages.ghc912; };

      # A nixpkgs overlay containing necessary overrides on dependencies added in rhine
      localDependenciesOverlay = final: prev:
        let
          # Overrides that are necessary because of dependencies not being up to date or fixed yet in nixpkgs.
          # Check on nixpkgs bumps whether some of these can be removed.
          temporaryHaskellOverrides = with prev.haskell.lib.compose; [
            (hfinal: hprev: {
              # Remove once nixpkgs has caught up
              monad-bayes = hprev.callHackageDirect {
                pkg = "monad-bayes";
                ver = "1.3.0.5";
                sha256 = "sha256-JK2Xya+xtPFIjJ6I+bb7dCU+JBJdhxGUMxB9jEZxZ78=";
              } { };
              # Transitive dependencies of monad-bayes
              vty = hprev.vty_6_5;
              brick = hprev.callHackageDirect {
                pkg = "brick";
                ver = "2.10";
                sha256 = "sha256-m1PvPySOuTZbcnCm4j7M7AihK0w8OGKumyRR3jU5nfw=";
              } { };

              changeset = hprev.callHackageDirect {
                pkg = "changeset";
                ver = "0.1.1";
                sha256 = "sha256-Y8F48Fe1m5YYnQ8IPcpS7rS19kcYqrnRC9RsToSOweI=";
              } { };
            })
            (hfinal: hprev: lib.optionalAttrs prev.stdenv.isDarwin {
              # For custom version: Don't test because tests don't work on Mac (https://github.com/tweag/monad-bayes/issues/368)
              monad-bayes = dontCheck hprev.monad-bayes;
              monad-schedule = dontCheck hprev.monad-schedule;
            })
            (hfinal: hprev: lib.optionalAttrs (lib.versionAtLeast hprev.ghc.version "9.10") {
              # Remove these after https://github.com/turion/rhine/issues/399
              gloss-rendering = doJailbreak hprev.gloss-rendering;
              gloss = doJailbreak hprev.gloss;

              # For rhine-tree
              websockets = doJailbreak hprev.websockets;
            })
            (hfinal: hprev: lib.optionalAttrs (lib.versionAtLeast hprev.ghc.version "9.12") {
              # Remove these after some nixpkgs bump
              statistics = doJailbreak hprev.statistics;
              monad-bayes = doJailbreak hprev.monad-bayes;
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
      rhine-tree-js = pkgs: import ./rhine-tree/nix { inherit pkgs overlay lib; };
    in
    {
      # Reexport the overlay so other downstream flakes can use it to develop rhine projects with low effort.
      overlays = {
        inherit dependenciesOverlay localOverlay;
        default = overlay;
      };

      # Format all *.nix files.
      # Usage: nix fmt
      formatter = forAllPlatforms (system: pkgs: pkgs.nixpkgs-fmt);

      # Build outputs.
      #
      # All packages, all GHC versions, docs and sdist:
      #   nix build                              # → result/ symlink to rhine-all
      #
      # Single package (default GHC, currently 9.12):
      #   nix build .#haskellPackages.rhine
      #
      # All packages for a specific GHC:
      #   nix build .#ghc912                     # or ghc94 / ghc96 / ghc98 / ghc910
      #
      # Hackage upload artefacts:
      #   nix build .#rhine-sdist                # → result/ contains *.tar.gz for each lib
      #   nix build .#rhine-docs                 # → result/ contains documentation tarballs
      #
      # rhine-tree browser app — GHC 9.12 WASM backend (runs fully in the browser):
      # Fully sandboxed: nix build compiles dommy.wasm via callCabal2nix cross-compilation.
      # No network access, no `cabal update`, no manual steps needed.
      #
      # Build everything (WASM + static files):
      #   nix build .#rhine-tree-js
      #   # result/ contains main.js, index.html, ghc_wasm_jsffi.js, dommy.wasm
      #
      # Serve (one-shot):
      #   nix run .#rhine-tree-app-wasm
      #   # Then open: http://localhost:8080
      #
      # Serve static files only:
      #   nix run .#rhine-tree-js-serve
      #   # Then open: http://localhost:8080
      packages = forAllPlatforms (system: pkgs:                {
        default = pkgs.rhine-all;
        rhine-tree-js = rhine-tree-js pkgs;
      } // lib.mapAttrs (ghcVersion: haskellPackages: pkgs.linkFarm "rhine-all-${ghcVersion}" (lib.genAttrs pnames (pname: haskellPackages.${pname}))) (hpsFor pkgs));

      # `nix run .#rhine-tree-app-wasm` — compile dommy.wasm and serve it.
      # Runs entirely on x86_64-linux (WASM toolchain constraint).
      apps.x86_64-linux.rhine-tree-app-wasm =
        let
          pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux.extend overlay;
          script = (rhine-tree-js pkgs).appScript;
        in
        { type = "app"; program = "${script}/bin/rhine-tree-app-wasm"; };

      # `nix run .#rhine-tree-js-serve` — serve the static files over HTTP
      # (no WASM compilation; useful for testing the loader before dommy.wasm is built).
      # Opening result/index.html directly in a browser will give a CORS error
      # because ES modules and fetch() require HTTP, not file://.
      apps.x86_64-linux.rhine-tree-js-serve =
        let
          pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux.extend overlay;
          script = (rhine-tree-js pkgs).serveScript;
        in
        { type = "app"; program = "${script}/bin/rhine-tree-js-serve"; };

      # The entire nixpkgs package set re-exported with the rhine overlay applied.
      # Useful for accessing packages that are not exposed as flake outputs.
      # Usage:
      #   nix build .#haskellPackages.rhine
      #   nix build .#haskell.packages.ghc98.rhine-bayes
      #   nix build .#rhine-sdist
      #   nix build .#rhine-docs
      legacyPackages = forAllPlatforms (system: pkgs: pkgs);

      # Development shells.
      #
      # Default shell (GHC 9.12) — for regular Haskell development:
      #   nix develop
      #   cabal build all
      #
      # Pick a specific GHC:
      #   nix develop .#ghc94   # or ghc96 / ghc98 / ghc910 / ghc912
      #
      # jsaddle-warp shell — fast browser iteration without WASM (native GHC):
      #   nix develop           # default shell is enough
      #   cabal run dommy-warp
      #   # Then open: http://localhost:8080
      #
      # WASM shell — optional, for interactive debugging only.
      #              `nix build .#rhine-tree-js` is now fully sandboxed.
      #   nix develop .#wasm
      #   wasm32-wasi-cabal build ...
      #
      # Format Haskell sources (fourmolu is available in every shell above):
      #   fourmolu -i $(git ls-files '*.hs')
      devShells = forAllPlatforms (systems: pkgs:  (mapAttrs
        (_: hp: hp.shellFor {
          packages = ps: map (pname: ps.${pname}) pnames;
          nativeBuildInputs = (with hp; lib.optional (lib.versionAtLeast hp.ghc.version "9.6")
            haskell-language-server
          ) ++ (with pkgs; [
            haskellPackages.fourmolu
            haskellPackages.cabal-gild
            cabal-install
          ]);
        })
        (hpsFor pkgs)) //
      {
        # GHC 9.12 WASM backend toolchain shell.
        # No longer needed for building — `nix build .#rhine-tree-js` is fully sandboxed.
        # Kept for interactive debugging / manual wasm32-wasi-cabal invocations.
        # Usage:
        #   nix develop .#wasm
        #   wasm32-wasi-cabal build ...
        wasm =
          let pkgs = inputs.ghc-wasm-meta.inputs.nixpkgs.legacyPackages.x86_64-linux;
          in
          pkgs.mkShell {
            packages = [
              inputs.ghc-wasm-meta.packages.x86_64-linux.all_9_12
              # pkgs.dart-sass
            ];
          };
        # GHCJS 9.10 cross-compilation shell (legacy; prefer the wasm shell).
        # Usage: nix develop .#js
        js =
          let
            pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux.pkgsCross.ghcjs.extend overlay;
            hp = pkgs.haskell.packages.ghc910;
          in
          hp.shellFor {
            packages = ps: map (pname: ps.${pname}) pnames;
            nativeBuildInputs = with hp; [
              cabal-gild
              cabal-install
              fourmolu
              haskell-language-server
            ];
          };
      });

      # Doesn't build on darwin
      # https://github.com/NixOS/nixpkgs/issues/367686
      supportedGhcs = lib.lists.removePrefix [ "ghc94" ] supportedGhcs;
    };
}
