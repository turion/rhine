{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PackageImports #-}

{- | Sums up natural numbers.

First create a lazy list [0, 1, 2, ...] and then sum over it.
-}
module SumMultirate where

import "base" Data.Functor ((<&>))
import "base" Data.Void (absurd)

import "criterion" Criterion.Main
import "vector-sized" Data.Vector.Sized qualified as V

import "automaton" Data.Automaton.Schedule.Trans (evalScheduleT)
import "rhine" FRP.Rhine
import "transformers" Control.Monad.Trans.Class (MonadTrans (..))

nMax :: Int
-- nMax = 1_000_000
nMax = 100_000 -- Multirate is much slower, so we reduce the number of iterations to get a result in a reasonable time

benchmarks :: Benchmark
benchmarks =
  bgroup
    "Sum Multirate"
    [ bench "rhine Busy Waitclock" $ whnfIO $ rhineBusyWaitclock nMax
    , bench "rhine Busy Busy" $ whnfIO $ rhineBusyBusy nMax
    , bench "rhine FixedStep 1:1" $ whnf rhineFixedStep11 nMax
    , bench "rhine FixedStep 1:1 IO" $ whnfIO $ rhineFixedStep11IO nMax
    , bench "rhine FixedStep 1000:1" $ whnf rhineFixedStep10001 nMax
    , bench "rhine FixedStep 1000:1 downsampleFixedStep" $ whnf rhineFixedStep11000downsampleFixedStep nMax
    , bench "rhine Trivial Trivial" $ whnf rhineTrivialTrivial nMax
    , bench "rhine single threaded" $ whnf rhineSingleThreaded nMax
    , bench "automaton reactimate" $ whnfIO $ automatonReactimate nMax
    , bench "direct" $ nf direct nMax
    ]

rhineBusyWaitclock :: Int -> IO Int
rhineBusyWaitclock n =
  fmap (either id absurd) $
    runExceptT $
      flow $
        (count @@ Busy) >-- (collect <&> reverse) --> traverseS consumer >-> arr (const ()) @@ waitClock @1
  where
    consumer = proc k -> do
      s <- sumN -< k
      if k < n
        then returnA -< ()
        else arrMCl throwE -< s

rhineBusyBusy :: Int -> IO Int
rhineBusyBusy n =
  fmap (either id absurd) $
    runExceptT $
      flow $
        (count @@ Busy) >-- (collect <&> reverse) --> traverseS consumer >-> arr (const ()) @@ Busy
  where
    consumer = proc k -> do
      s <- sumN -< k
      if k < n
        then returnA -< ()
        else arrMCl throwE -< s

rhineFixedStep11 :: Int -> Int
rhineFixedStep11 n =
  either id absurd $
    runExcept $
      evalScheduleT $
        flow $
          (count @@ FixedStep @1) >-- (collect <&> reverse) --> traverseS consumer >-> arr (const ()) @@ FixedStep @1
  where
    consumer = proc k -> do
      s <- sumN -< k
      if k < n
        then returnA -< ()
        else arrMCl $ lift . throwE -< s

rhineFixedStep11IO :: Int -> IO Int
rhineFixedStep11IO n =
  fmap (either id absurd) $
    runExceptT $
      evalScheduleT $
        flow $
          (count @@ FixedStep @1) >-- (collect <&> reverse) --> traverseS consumer >-> arr (const ()) @@ FixedStep @1
  where
    consumer = proc k -> do
      s <- sumN -< k
      if k < n
        then returnA -< ()
        else arrMCl $ lift . throwE -< s

rhineFixedStep10001 :: Int -> Int
rhineFixedStep10001 n =
  either id absurd $
    runExcept $
      evalScheduleT $
        flow $
          (count @@ FixedStep @1) >-- (collect <&> reverse) --> traverseS consumer >-> arr (const ()) @@ FixedStep @1000
  where
    consumer = proc k -> do
      s <- sumN -< k
      if k < n
        then returnA -< ()
        else arrMCl $ lift . throwE -< s

rhineFixedStep11000downsampleFixedStep :: Int -> Int
rhineFixedStep11000downsampleFixedStep n =
  either id absurd $
    runExcept $
      evalScheduleT $
        flow $
          (count @@ FixedStep @1) >-- (downsampleFixedStep <&> V.reverse) --> traverseS consumer >-> arr (const ()) @@ FixedStep @1000
  where
    consumer = proc k -> do
      s <- sumN -< k
      if k < n
        then returnA -< ()
        else arrMCl $ lift . throwE -< s

rhineTrivialTrivial :: Int -> Int
rhineTrivialTrivial n =
  either id absurd $
    runExcept $
      flow $
        (count @@ Trivial) >-- (collect <&> reverse) --> traverseS consumer >-> arr (const ()) @@ Trivial
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
