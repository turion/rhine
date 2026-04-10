{ pkgs, overlay, lib, wasmPkgs }:
# rhine-tree browser app — static web files + build script.
#
# `nix build .#rhine-tree-js` produces a directory containing:
#   - main.js      : ES-module WASI loader (loads dommy.wasm at runtime)
#   - index.html   : minimal HTML page
#   - build.sh     : script that compiles dommy.wasm when run inside `nix develop .#wasm`
#
# WASM compilation cannot run inside the Nix sandbox because wasm32-wasi-cabal
# needs live network access to the Hackage index (the same constraint affects
# every project in https://github.com/haskell-wasm).
#
# Full workflow:
#   nix build .#rhine-tree-js          # build the static files + build.sh
#   cp -r --no-preserve=mode result/ dist/
#   nix develop .#wasm                 # enter the WASM toolchain shell
#   cd dist && bash build.sh           # compiles dommy.wasm into dist/
#   # exit the shell, then serve:
#   python3 -m http.server 8080 --directory dist/
#   # open http://localhost:8080
let
  npmDeps = wasmPkgs.npm-deps;
  wasmTools = wasmPkgs.all_9_12;

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

  # Shell script that compiles dommy.wasm.
  # Must be run from inside `nix develop .#wasm` with the repo root as $PWD.
  # Argument: destination directory (defaults to $PWD/dist).
  buildSh = pkgs.writeShellScript "build.sh" ''
    set -euo pipefail

    DEST="''${1:-$PWD/dist}"
    # When called via appScript the CWD is the repo root; fall back to git otherwise.
    REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || echo "$PWD")"

    if ! command -v wasm32-wasi-cabal &>/dev/null; then
      echo "ERROR: wasm32-wasi-cabal not found." >&2
      echo "Run this script from inside: nix develop .#wasm" >&2
      exit 1
    fi

    # Write a minimal cabal.project in a subdirectory of $DEST that only
    # includes rhine-tree, avoiding monorepo packages with TUI deps
    # unbuildable under WASM (e.g. brick/vty pulled in by rhine-bayes).
    WASM_PROJ_DIR="$DEST/wasm-project"
    mkdir -p "$WASM_PROJ_DIR"
    echo "packages: $REPO_ROOT/rhine-tree/rhine-tree.cabal" > "$WASM_PROJ_DIR/cabal.project"

    # Update the Hackage index only if it is absent; wasm32-wasi-cabal update
    # is known to segfault on some systems when the index is already up to date.
    HACKAGE_INDEX="$HOME/.cabal/packages/hackage.haskell.org/01-index.tar"
    if [[ ! -f "$HACKAGE_INDEX" ]]; then
      echo "==> Updating Hackage index (first run)..."
      wasm32-wasi-cabal update
    else
      echo "==> Hackage index already present, skipping update."
    fi

    echo "==> Building exe:dommy..."
    wasm32-wasi-cabal build --project-dir="$WASM_PROJ_DIR" exe:dommy -j"$(nproc)"

    WASM_BIN="$(wasm32-wasi-cabal list-bin --project-dir="$WASM_PROJ_DIR" exe:dommy 2>/dev/null | tail -1)"
    echo "==> WASM binary: $WASM_BIN"

    echo "==> Running post-link.mjs..."
    LIBDIR="$(wasm32-wasi-ghc --print-libdir)"
    export NODE_PATH="${npmDeps}/lib/node_modules"
    node "$LIBDIR/post-link.mjs" --input "$WASM_BIN" --output "$DEST/ghc_wasm_jsffi.js"

    echo "==> Optimising with wasm-opt..."
    wasm-opt -O2 --enable-bulk-memory "$WASM_BIN" -o "$DEST/dommy.wasm"

    echo "==> Done. Serve with:"
    echo "    python3 -m http.server 8080 --directory $DEST/"
  '';

  # The static files package (produced by `nix build .#rhine-tree-js`).
  staticFiles = pkgs.runCommand "rhine-tree-js"
    { nativeBuildInputs = [ pkgs.python3 ]; } ''
    mkdir -p $out
    cp ${mainJs}    $out/main.js
    cp ${indexHtml} $out/index.html
    cp ${buildSh}   $out/build.sh
    chmod +x        $out/build.sh
    # Convenience: serve the directory over HTTP so the browser can load it.
    # Usage: nix build .#rhine-tree-js && bash result/serve.sh
    cat > $out/serve.sh <<'EOF'
#!/usr/bin/env python3
import http.server, os, sys
port = int(sys.argv[1]) if len(sys.argv) > 1 else 8080
os.chdir(os.path.dirname(os.path.abspath(__file__)))
print(f"Serving http://localhost:{port}/  (Ctrl-C to stop)")
http.server.test(HandlerClass=http.server.SimpleHTTPRequestHandler, port=port)
EOF
    chmod +x $out/serve.sh
  '';

  # Serve the static files over HTTP for browser testing (no WASM compilation).
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

  # All-in-one app script for `nix run .#rhine-tree-app-wasm`.
  #
  # Intentionally kept thin: it does NOT embed the WASM toolchain in its own
  # closure (that would require downloading gigabytes just to evaluate the app
  # derivation).  Instead it delegates to `nix develop .#wasm --command …`
  # which fetches the toolchain lazily at runtime, outside any Nix sandbox, so
  # `wasm32-wasi-cabal update` can reach the network.
  appScript = pkgs.writeShellApplication {
    name = "rhine-tree-app-wasm";
    runtimeInputs = [ pkgs.nix pkgs.git pkgs.python3 ];
    text = ''
      set -euo pipefail

      # Must be run from inside the rhine repository.
      REPO_ROOT="$(git rev-parse --show-toplevel)"

      # Prepare a writable dist directory next to the repo.
      DIST="$(mktemp -d -t rhine-tree-js.XXXXXX)"
      trap 'rm -rf "$DIST"' EXIT

      echo "==> Copying static files to $DIST..."
      cp ${mainJs}    "$DIST/main.js"
      cp ${indexHtml} "$DIST/index.html"
      cp ${buildSh}   "$DIST/build.sh"

      echo "==> Entering WASM shell and running build.sh..."
      # `nix develop .#wasm --command …` puts all_9_12 tools on PATH and then
      # runs the command without a sandbox, so network access works fine.
      (cd "$REPO_ROOT" && nix develop .#wasm --command bash "$DIST/build.sh" "$DIST")

      echo ""
      echo "==> Serving at http://localhost:8080  (Ctrl-C to stop)"
      echo "    Open: http://localhost:8080"
      python3 -m http.server 8080 --directory "$DIST"
    '';
  };

in
{ inherit staticFiles appScript serveScript; }
