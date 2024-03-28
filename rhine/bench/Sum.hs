{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PackageImports #-}

{- | Sums up natural numbers.

First create a lazy list [0, 1, 2, ...] and then sum over it.
Most of the implementations really benchmark 'embed', as the lazy list is created using it.
-}
module Sum where

import "base" Control.Monad (foldM)
import "base" Data.Functor.Identity
import "base" Data.Void (absurd)

import "criterion" Criterion.Main

import "dunai" Data.MonadicStreamFunction as Dunai

import "automaton" Data.Stream as Stream (StreamT (..))
import "automaton" Data.Stream.Optimized (OptimizedStreamT (Stateful))
import "automaton" Data.Stream.Result (Result (..))
import "rhine" FRP.Rhine as Rhine

nMax :: Int
nMax = 1_000_000

benchmarks :: Benchmark
benchmarks =
  bgroup
    "Sum"
    [ bench "rhine" $ nf rhine nMax
    , bench "rhine flow" $ nf rhineFlow nMax
    , bench "dunai" $ nf dunai nMax
    , bench "automaton" $ nf automaton nMax
    , bench "direct" $ nf direct nMax
    , bench "direct monad" $ nf directM nMax
    ]

rhine :: Int -> Int
rhine n = sum $ runIdentity $ Rhine.embed Rhine.count $ replicate n ()

-- FIXME separate ticket to improve performance of this
rhineFlow :: Int -> Int
rhineFlow n =
  either id absurd $
    flow $
      (@@ Trivial) $ proc () -> do
        k <- Rhine.count -< ()
        s <- Rhine.sumN -< k
        if k < n
          then returnA -< ()
          else arrMCl Left -< s

dunai :: Int -> Int
dunai n = sum $ runIdentity $ Dunai.embed Dunai.count $ replicate n ()

automaton :: Int -> Int
automaton n = sum $ runIdentity $ Rhine.embed myCount $ replicate n ()
  where
    myCount :: Automaton Identity () Int
    myCount =
      Automaton $
        Stateful
          StreamT
            { state = 1
            , Stream.step = \s -> return $! Result (s + 1) s
            }

direct :: Int -> Int
direct n = sum [0 .. n]

directM :: Int -> Int
directM n = runIdentity $ foldM (\a b -> return $ a + b) 0 [0 .. n]
