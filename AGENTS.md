Don't create commands referencing /dev/null!

## WASM / WARP dev setup

- `nix build .#rhine-tree-js` (x86_64-linux only) — builds the complete browser WASM app (dommy.wasm + ghc_wasm_jsffi.js + main.js + index.html) fully in-sandbox.
- `nix develop .#wasm` (x86_64-linux only) — drops into a shell with the GHC WASM toolchain (`wasm32-wasi-ghc`, `wasm32-wasi-cabal`, etc.) for manual iteration.
- `nix develop .#js` (x86_64-linux only) — GHCJS cross-compilation shell (JS-compatible packages only: excludes rhine-bayes, rhine-gloss, rhine-terminal).
- `cabal run dommy-warp` (any platform, normal GHC) — serves the app via jsaddle-warp at http://localhost:8080 for fast native development without WASM.
