{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PackageImports #-}

{- | Sums up natural numbers.

First create a lazy list [0, 1, 2, ...] and then sum over it.
-}
module SumMultirate where

import "base" Data.Void (absurd)

import "criterion" Criterion.Main

import "rhine" FRP.Rhine

nMax :: Int
-- nMax = 1_000_000
nMax = 100_000 -- Multirate is much slower, so we reduce the number of iterations to get a result in a reasonable time

benchmarks :: Benchmark
benchmarks =
  bgroup
    "Sum Multirate"
    [ bench "rhine flow" $ whnfIO $ rhineFlow nMax
    , bench "rhine single threaded" $ whnf rhineSingleThreaded nMax
    , bench "automaton reactimate" $ whnfIO $ automatonReactimate nMax
    , bench "direct" $ nf direct nMax
    ]

rhineFlow :: Int -> IO Int
rhineFlow n =
  fmap (either id absurd) $
    runExceptT $
      flow $
        (count @@ Busy) >-- collect --> traverseS consumer >-> arr (const ()) @@ waitClock @1
  where
    consumer = proc k -> do
      s <- sumN -< k
      if k < n
        then returnA -< ()
        else arrMCl throwE -< s

rhineSingleThreaded :: Int -> Int
rhineSingleThreaded n =
  either id absurd $
    flow $
      (@@ Trivial) $ proc () -> do
        k <- count -< ()
        s <- sumN -< k
        if k < n
          then returnA -< ()
          else arrMCl Left -< s

-- This is a single threaded comparison, so expected to be faster
automatonReactimate :: Int -> IO Int
automatonReactimate n =
  fmap (either id absurd) $
    runExceptT $
      reactimate $ proc () -> do
        k <- count -< ()
        s <- sumN -< k
        if k < n
          then returnA -< ()
          else arrM throwE -< s

-- There is no good direct comparison since we can't model the scheduling well
-- So I'll just wastefully create a list of list and concatenate it again... maybe it hits some performance
direct :: Int -> Int
direct n = sum $ concatMap pure [0 .. n]
