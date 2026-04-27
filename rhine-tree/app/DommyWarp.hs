-- | jsaddle-warp entry point for the rhine-tree demo app.
-- Build with: cabal build exe:dommy-warp
-- Run with:   cabal run dommy-warp
-- Then open:  http://localhost:8080
module Main where

import FRP.Rhine.Tree.App (mainJSM)
import Language.Javascript.JSaddle.Warp (run)

main :: IO ()
main = do
  putStrLn "Let's go"
  run 8080 mainJSM
