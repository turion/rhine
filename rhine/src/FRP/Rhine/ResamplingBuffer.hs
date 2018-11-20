{- |
This module introduces 'ResamplingBuffer's,
which are primitives that consume and produce data at different rates.
Just as schedules form the boundaries between different clocks,
(resampling) buffers form the boundaries between
synchronous signal functions ticking at different speeds.
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module FRP.Rhine.ResamplingBuffer
  ( module FRP.Rhine.ResamplingBuffer
  , module FRP.Rhine.Clock
  )
  where

-- rhine
import FRP.Rhine.Clock


-- base
import Control.Arrow (second)

-- A quick note on naming conventions, to whoever cares:
-- . Call a single clock @cl@.
-- . Call several clocks @cl1@, @cl2@ etc. in most situations.
-- . Call it @cla@, @clb@ etc. when they are 'In' or 'Out' clocks,
-- i.e. associated to particular boundary types @a@, @b@ etc.,

{- | A stateful buffer from which one may 'get' a value,
or to which one may 'put' a value,
depending on the clocks.
`ResamplingBuffer`s can be clock-polymorphic,
or specific to certain clocks.

* 'm': Monad in which the 'ResamplingBuffer' may have side effects
* 'cla': The clock at which data enters the buffer
* 'clb': The clock at which data leaves the buffer
* 'a': The input type
* 'b': The output type
-}
data ResamplingBuffer m cla clb a b = ResamplingBuffer
  { put
      :: TimeInfo cla
      -> a
      -> m (   ResamplingBuffer m cla clb a b)
    -- ^ Store one input value of type 'a' at a given time stamp,
    --   and return a continuation.
  , get
      :: TimeInfo clb
      -> m (b, ResamplingBuffer m cla clb a b)
    -- ^ Retrieve one output value of type 'b' at a given time stamp,
    --   and a continuation.
  }

-- | A type synonym to allow for abbreviation.
type ResBuf m cla clb a b = ResamplingBuffer m cla clb a b


-- | Hoist a 'ResamplingBuffer' along a monad morphism.
hoistResamplingBuffer
  :: (Monad m1, Monad m2)
  => (forall c. m1 c -> m2 c)
  -> ResamplingBuffer m1 cla clb a b
  -> ResamplingBuffer m2 cla clb a b
hoistResamplingBuffer hoist ResamplingBuffer {..} = ResamplingBuffer
  { put = (((hoistResamplingBuffer hoist <$>) . hoist) .) . put
  , get = (second (hoistResamplingBuffer hoist) <$>) . hoist . get
  }

-- | Hoist a 'ResamplingBuffer' along a monad morphism.
hoistResamplingBufferAndClocks
  :: (Monad m1, Monad m2)
  => (forall c. m1 c -> m2 c)
  -> ResamplingBuffer m1 cla clb a b
  -> ResamplingBuffer m2 (HoistClock m1 m2 cla) (HoistClock m1 m2 clb) a b
hoistResamplingBufferAndClocks hoist ResamplingBuffer {..} = ResamplingBuffer
  { put = (((hoistResamplingBufferAndClocks hoist <$>) . hoist) .) . put . retag id
  , get = (second (hoistResamplingBufferAndClocks hoist) <$>) . hoist . get . retag id
  }
