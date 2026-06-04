{ pkgs, nixpkgsSrc, lib, ghcWasmMeta }:
# rhine-tree browser app — WASM cross-compiled fully inside the Nix sandbox.
#
# `nix build .#rhine-tree-js` produces a directory containing:
#   - main.js          : ES-module WASI loader (loads dommy.wasm at runtime)
#   - ghc_wasm_jsffi.js: generated JSFFI glue (produced by post-link.mjs)
#   - dommy.wasm       : the compiled WASM binary (optimised with wasm-opt)
#   - index.html       : minimal HTML page
#
# Everything is built in-sandbox via callCabal2nix cross-compilation.
# No network access, no `cabal update`, no wasm32-wasi-cabal needed at runtime.
# Approach adapted from https://github.com/ners/nix-wasm (GHC 9.12).
let
  system = pkgs.stdenv.system;
  ghc = "ghc912";
  targetPrefix = "wasm32-wasi-";

  # Instantiate nixpkgs for the wasm32-wasi cross target.
  # This mirrors the ners/nix-wasm GHC 9.12 approach exactly.
  wasmPkgs = import nixpkgsSrc {
    inherit system;
    crossSystem = lib.systems.elaborate lib.systems.examples.wasi32 // { isStatic = false; };
    config.replaceCrossStdenv = { buildPackages, baseStdenv }:
      buildPackages.stdenvNoCC.override {
        inherit (baseStdenv) buildPlatform hostPlatform targetPlatform;
        cc = ghcWasmMeta.packages.${system}.all_9_12 // {
          isGNU = false;
          isClang = true;
          libc = ghcWasmMeta.packages.${system}.wasi-sdk.overrideAttrs
            (attrs: { pname = attrs.name; version = "unstable1"; });
          inherit targetPrefix;
          bintools = ghcWasmMeta.packages.${system}.all_9_12 // {
            inherit targetPrefix;
            bintools = ghcWasmMeta.packages.${system}.all_9_12 // { inherit targetPrefix; };
          };
        };
      };
    crossOverlays = [
      (final: prev: {
        cabal-install = ghcWasmMeta.packages.${system}.wasm32-wasi-cabal-9_12;
        haskell = (prev.haskell.override (old: {
          buildPackages = lib.recursiveUpdate old.buildPackages {
            haskell.compiler.${ghc} =
              ghcWasmMeta.packages.${system}.wasm32-wasi-ghc-9_12 // { inherit targetPrefix; };
          };
        })) // {
          packageOverrides = lib.composeManyExtensions [
            prev.haskell.packageOverrides
            (hfinal: hprev: {
              ghc = ghcWasmMeta.packages.${system}.wasm32-wasi-ghc-9_12 // {
                inherit
                  (nixpkgsSrc.legacyPackages.${system}.haskell.packages.${ghc}.ghc)
                  version haskellCompilerName;
                inherit targetPrefix;
              };

              mkDerivation = args: (hprev.mkDerivation (args // {
                enableLibraryProfiling = false;
                enableSharedLibraries = true;
                enableStaticLibraries = false;
                enableExternalInterpreter = false;
                doBenchmark = false;
                doHaddock = false;
                doCheck = false;
                jailbreak = true;
                configureFlags = (args.configureFlags or [ ]) ++ [
                  "--with-ld=${prev.stdenv.cc.bintools}/bin/lld"
                  "--with-ar=${prev.stdenv.cc.bintools}/bin/ar"
                  "--with-strip=${prev.stdenv.cc.bintools}/bin/strip"
                ];
                setupHaskellDepends = (args.setupHaskellDepends or [ ]) ++ [
                  ghcWasmMeta.packages.${system}.wasi-sdk
                ];
                preBuild = "${args.preBuild or ""}\nexport NIX_CC=$CC";
              })).overrideAttrs (attrs: {
                name = "${attrs.pname}-${targetPrefix}${attrs.version}";
                preSetupCompilerEnvironment = "export CC_FOR_BUILD=$CC";
              });

              # zlib needs the C library as a build dependency under WASM
              zlib = prev.haskell.lib.compose.addBuildDepend hprev.zlib-clib hprev.zlib;

              # Local monorepo packages — cross-compiled from source
              time-domain = hfinal.callCabal2nix "time-domain" ../../time-domain { };
              monad-schedule = hfinal.callCabal2nix "monad-schedule" ../../monad-schedule { };
              automaton = hfinal.callCabal2nix "automaton" ../../automaton { };
              automaton-lens = hfinal.callCabal2nix "automaton-lens" ../../automaton-lens { };
              rhine = hfinal.callCabal2nix "rhine" ../../rhine { };
              # Enable the `wasm` flag so dommy-warp (jsaddle-warp → warp → crypton → basement)
              # is disabled — basement's cbits don't have a WASI code path.
              rhine-tree = prev.haskell.lib.compose.enableCabalFlag "wasm"
                (hfinal.callCabal2nix "rhine-tree" ../../rhine-tree { });
            })
          ];
        };
      })
    ];
  };

  wasmHaskellPkgs = wasmPkgs.haskell.packages.${ghc};

  # The dommy executable, compiled to WASM.
  # callCabal2nix automatically depends on the local overrides above.
  dommyWasmExe = wasmHaskellPkgs.rhine-tree;

  # The raw .wasm binary produced by the GHC WASM backend.
  dommyWasmBin = pkgs.runCommand "dommy-raw.wasm" { } ''
    find ${dommyWasmExe}/bin -name "dommy" -print0 \
      | xargs -0 -I{} cp {} $out
  '';

  # Run post-link.mjs to generate ghc_wasm_jsffi.js and the linked .wasm.
  # post-link.mjs lives in GHC's lib directory inside the WASM toolchain.
  postLinkOutputs = pkgs.runCommand "dommy-post-link"
    {
      nativeBuildInputs = [
        ghcWasmMeta.packages.${system}.all_9_12
        pkgs.nodejs
      ];
    } ''
    mkdir -p $out
    LIBDIR="$(wasm32-wasi-ghc --print-libdir)"
    NODE_PATH="${ghcWasmMeta.packages.${system}.npm-deps}/lib/node_modules" \
      node "$LIBDIR/post-link.mjs" \
        --input ${dommyWasmBin} \
        --output $out/ghc_wasm_jsffi.js
  '';

  # Optimise the linked WASM binary with wasm-opt.
  dommyWasm = pkgs.runCommand "dommy.wasm"
    {
      nativeBuildInputs = [ ghcWasmMeta.packages.${system}.all_9_12 ];
    } ''
    wasm-opt -O2 --enable-bulk-memory \
      ${postLinkOutputs}/dommy.wasm -o $out
  '';

  # ES-module loader: instantiates the WASM binary + WASI shim in the browser.
  # Uses @bjorn3/browser_wasi_shim via jsDelivr CDN (bare npm specifiers are not
  # valid in browsers without a bundler or importmap — use the full CDN URL instead).
  mainJs = pkgs.writeTextFile {
    name = "main.js";
    text = ''
      // ES-module loader for the rhine-tree dommy WASM app.
      // @bjorn3/browser_wasi_shim is loaded via jsDelivr CDN so no bundler is needed.
      import { WASI, OpenFile, File, ConsoleStdout } from "https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/dist/index.js";
      import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

      const fds = [
        new OpenFile(new File([])),   // stdin  (unused)
        ConsoleStdout.lineBuffered(msg => console.log("[stdout] " + msg)),
        ConsoleStdout.lineBuffered(msg => console.warn("[stderr] " + msg)),
      ];
      const wasi = new WASI([], [], fds, { debug: false });

      const instance_exports = {};

      const { instance } = await WebAssembly.instantiateStreaming(
        fetch("dommy.wasm"),
        {
          wasi_snapshot_preview1: wasi.wasiImport,
          ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
        }
      );

      Object.assign(instance_exports, instance.exports);

      // Initialise the WASI reactor (must be called exactly once before hs_start)
      wasi.initialize(instance);

      // Run the Haskell application
      await instance.exports.hs_start();
    '';
  };

  indexHtml = pkgs.writeTextFile {
    name = "index.html";
    text = ''
      <!DOCTYPE html>
      <html lang="en">
        <head>
          <meta charset="UTF-8" />
          <meta name="viewport" content="width=device-width, initial-scale=1.0" />
          <title>rhine-tree dommy</title>
          <script type="module" src="main.js"></script>
        </head>
        <body>
          <!-- The rhine-tree application mounts itself here via jsaddle-wasm -->
        </body>
      </html>
    '';
  };

  # The static files package (produced by `nix build .#rhine-tree-js`).
  # Contains everything needed to serve the app directly — no further build steps required.
  staticFiles = pkgs.runCommand "rhine-tree-js" { } ''
    mkdir -p $out
    cp ${mainJs}                         $out/main.js
    cp ${indexHtml}                      $out/index.html
    cp ${postLinkOutputs}/ghc_wasm_jsffi.js $out/ghc_wasm_jsffi.js
    cp ${dommyWasm}                      $out/dommy.wasm
  '';

  # Serve the static files over HTTP for browser testing.
  # Usage: nix run .#rhine-tree-js-serve
  serveScript = pkgs.writeShellApplication {
    name = "rhine-tree-js-serve";
    runtimeInputs = [ pkgs.python3 ];
    text = ''
      PORT="''${1:-8080}"
      echo "Serving http://localhost:$PORT/  (Ctrl-C to stop)"
      python3 -m http.server "$PORT" --directory ${staticFiles}
    '';
  };

  # All-in-one script: build is already done by Nix, just serve.
  # Usage: nix run .#rhine-tree-app-wasm
  appScript = pkgs.writeShellApplication {
    name = "rhine-tree-app-wasm";
    runtimeInputs = [ pkgs.python3 ];
    text = ''
      PORT="''${1:-8080}"
      echo "==> Serving rhine-tree WASM app at http://localhost:$PORT/  (Ctrl-C to stop)"
      echo "    Open: http://localhost:$PORT/"
      python3 -m http.server "$PORT" --directory ${staticFiles}
    '';
  };

in
{ inherit staticFiles appScript serveScript; }
