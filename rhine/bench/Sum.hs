{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PackageImports #-}

{- | Sums up natural numbers.

First create a lazy list [0, 1, 2, ...] and then sum over it.
Most of the implementations really benchmark 'embed', as the lazy list is created using it.
-}
module Sum where

import "base" Control.Monad (foldM)
import "base" Data.Either (fromLeft)
import "base" Data.Functor.Identity
import "base" Data.Void (absurd)
import "criterion" Criterion.Main

import "automaton" Data.Stream as Stream (StreamT (..))
import qualified "automaton" Data.Stream as Stream (reactimate)
import "automaton" Data.Stream.Optimized (OptimizedStreamT (Stateful))
import "rhine" FRP.Rhine

nMax :: Int
nMax = 1_000_000

benchmarks :: Benchmark
benchmarks =
  bgroup
    "Sum"
    [ bench "rhine embed" $ nf rhine nMax
    , bench "rhine flow" $ nf rhineFlow nMax
    , bench "rhine flow IO" $ nfAppIO rhineMS nMax
    , bench "automaton embed" $ nf automaton nMax
    , bench "automatonNoEmbed" $ nf automatonNoEmbed nMax
    , bench "automatonEmbed" $ nf automatonEmbed nMax
    , bench "automatonNoEmbedInlined" $ nf automatonNoEmbedInlined nMax
    , bench "direct" $ nf direct nMax
    , bench "direct monad" $ nf directM nMax
    ]

rhine :: Int -> Int
rhine n = sum $ runIdentity $ embed count $ replicate n ()

-- FIXME separate ticket to improve performance of this
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

myclock :: IOClock (ExceptT Int IO) (Millisecond 0)
myclock = ioClock waitClock

rhineMS :: Int -> IO Int
rhineMS n =
  fmap (either id absurd) $
  runExceptT $
    flow $
      (@@ myclock) $ proc () -> do
        k <- count -< ()
        s <- sumN -< k
        if k < n
          then returnA -< ()
          else throwS -< s

automaton :: Int -> Int
automaton n = sum $ runIdentity $ embed myCount $ replicate n ()
  where
    myCount :: Automaton Identity () Int
    myCount =
      Automaton $
        Stateful
          StreamT
            { state = 1
            , Stream.step = \s -> return $! Result (s + 1) s
            }

automatonEmbed :: Int -> Int
automatonEmbed n = fromLeft (error "nope") $ flip embed (repeat ()) $ proc () -> do
  k <- count -< ()
  s <- sumN -< k
  if k < n
    then returnA -< ()
    else arrM Left -< s

automatonNoEmbed :: Int -> Int
automatonNoEmbed n = either id absurd $ reactimate $ proc () -> do
  k <- count -< ()
  s <- sumN -< k
  if k < n
    then returnA -< ()
    else arrM Left -< s

automatonNoEmbedInlined :: Int -> Int
automatonNoEmbedInlined k = either id absurd $ Stream.reactimate StreamT
            { state = (1, 0)
            , Stream.step = \(n, s) ->
              let n' = n + 1
                  s' = s + n
              in if n' > k then Left s' else return $! Result (n', s') ()
            }


direct :: Int -> Int
direct n = sum [0 .. n]

directM :: Int -> Int
directM n = runIdentity $ foldM (\a b -> return $ a + b) 0 [0 .. n]
