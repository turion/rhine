module FRP.Rhine.ResamplingBuffer where

import FRP.Rhine.Clock
import FRP.Rhine.TimeDomain
import FRP.Rhine.Schedule

data ResamplingBuffer m c1 c2 a b = ResamplingBuffer
  { put :: TimeInfo c1 -> a -> m (   ResamplingBuffer m c1 c2 a b)
  , get :: TimeInfo c2      -> m (b, ResamplingBuffer m c1 c2 a b)
  }


timelessResamplingBuffer :: Monad m
                         => (s -> a -> m s)
                         -> (s -> m (b, s))
                         -> s
                         -> ResamplingBuffer m c1 c2 a b
timelessResamplingBuffer tput tget s = ResamplingBuffer put get
  where
    put _ a = timelessResamplingBuffer tput tget <$> tput s a
    get _   = do
      (b, s') <- tget s
      return (b, timelessResamplingBuffer tput tget s')

trivialResamplingBuffer :: Monad m => cl
                        -> ResamplingBuffer m (Rightmost cl) (Leftmost cl) () ()
trivialResamplingBuffer cl = go
  where
    go  = ResamplingBuffer put get
    put _ _ = return      go
    get _   = return ((), go)
