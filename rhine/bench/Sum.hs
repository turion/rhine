{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PackageImports #-}

{- | Sums up natural numbers.

First create a lazy list [0, 1, 2, ...] and then sum over it.
-}
module Sum where

import "base" Control.Monad (foldM)
import "base" Data.Functor.Identity
import "base" Data.Void (absurd)

import "criterion" Criterion.Main

import "rhine" FRP.Rhine

nMax :: Int
nMax = 1_000_000

benchmarks :: Benchmark
benchmarks =
  bgroup
    "Sum"
    [ bench "rhine flow" $ nf rhineFlow nMax
    , bench "rhine IO" $ whnfIO rhineIO
    , bench "automaton reactimate" $ nf automatonReactimate nMax
    , bench "automaton reactimate IO" $ whnfIO automatonReactimateIO
    , bench "direct" $ nf direct nMax
    , bench "direct monad" $ nf directM nMax
    ]

rhineFlow :: Int -> Int
rhineFlow n =
  either id absurd $
    flow $
      (@@ Trivial) $ proc () -> do
        k <- count -< ()
        s <- sumN -< k
        if k < n
          then returnA -< ()
          else arrMCl Left -< s

rhineIO :: IO Int
rhineIO = fmap (either id absurd) $
  runExceptT $
    flow $
      (@@ Trivial) $ proc () -> do
        k <- count -< ()
        s <- sumN -< k
        if k < nMax
          then returnA -< ()
          else throwS -< s

automatonReactimate :: Int -> Int
automatonReactimate n =
  either id absurd $
    reactimate $ proc () -> do
      k <- count -< ()
      s <- sumN -< k
      if k < n
        then returnA -< ()
        else arrM Left -< s

automatonReactimateIO :: IO Int
automatonReactimateIO = fmap (either id absurd) $
  runExceptT $
    reactimate $
      proc () -> do
        k <- count -< ()
        s <- sumN -< k
        if k < nMax
          then returnA -< ()
          else arrM throwE -< s

direct :: Int -> Int
direct n = sum [0 .. n]

directM :: Int -> Int
directM n = runIdentity $ foldM (\a b -> return $ a + b) 0 [0 .. n]
