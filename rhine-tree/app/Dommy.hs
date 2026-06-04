{-# LANGUAGE GHCForeignImportPrim #-}
-- | WASM entry point for the rhine-tree demo app.
-- Build with: wasm32-wasi-cabal build exe:dommy
--
-- The `foreign export javascript` pragma is only meaningful with the GHC WASM
-- backend. The JS loader (main.js) calls `hs_start` to boot the application.
foreign export javascript "hs_start" main :: IO ()

import FRP.Rhine.Tree.App (mainJSM)
import Language.Javascript.JSaddle.Wasm qualified as JSaddle.Wasm

main :: IO ()
main = JSaddle.Wasm.run mainJSM
