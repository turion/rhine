{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
This module introduces 'ResamplingBuffer's,
which are primitives that consume and produce data at different rates.
Just as schedules form the boundaries between different clocks,
(resampling) buffers form the boundaries between
synchronous signal functions ticking at different speeds.
-}
module FRP.Rhine.ResamplingBuffer (
  module FRP.Rhine.ResamplingBuffer,
  module FRP.Rhine.Clock,
)
where

-- profunctors
import Data.Profunctor (Profunctor (..))

-- automaton
import Data.Stream.Result

-- rhine
import FRP.Rhine.Clock

-- A quick note on naming conventions, to whoever cares:
-- . Call a single clock @cl@.
-- . Call several clocks @cl1@, @cl2@ etc. in most situations.
-- . Call it @cla@, @clb@ etc. when they are 'In' or 'Out' clocks,
-- i.e. associated to particular boundary types @a@, @b@ etc.,

{- | A stateful buffer from which one may 'get' a value,
or to which one may 'put' a value,
depending on the clocks.
'ResamplingBuffer's can be clock-polymorphic,
or specific to certain clocks.

* 'm': Monad in which the 'ResamplingBuffer' may have side effects
* 'cla': The clock at which data enters the buffer
* 'clb': The clock at which data leaves the buffer
* 'a': The input type
* 'b': The output type
-}
data ResamplingBuffer m cla clb a b
  = forall s.
  ResamplingBuffer
  { buffer :: s
  -- ^ The internal state of the buffer.
  , put ::
      TimeInfo cla ->
      a ->
      s ->
      m s
  -- ^ Store one input value of type 'a' at a given time stamp,
  --   and return an updated state.
  , get ::
      TimeInfo clb ->
      s ->
      m (Result s b)
  -- ^ Retrieve one output value of type 'b' at a given time stamp,
  --   and an updated state.
  }

-- | A type synonym to allow for abbreviation.
type ResBuf m cla clb a b = ResamplingBuffer m cla clb a b

-- | Hoist a 'ResamplingBuffer' along a monad morphism.
hoistResamplingBuffer ::
  (Monad m1, Monad m2) =>
  (forall c. m1 c -> m2 c) ->
  ResamplingBuffer m1 cla clb a b ->
  ResamplingBuffer m2 cla clb a b
hoistResamplingBuffer morph ResamplingBuffer {..} =
  ResamplingBuffer
    { put = ((morph .) .) . put
    , get = (morph .) . get
    , buffer
    }

instance (Functor m) => Profunctor (ResamplingBuffer m cla clb) where
  lmap f ResamplingBuffer {put, get, buffer} =
    ResamplingBuffer
      { put = (. f) <$> put
      , get
      , buffer
      }
  rmap = fmap

instance (Functor m) => Functor (ResamplingBuffer m cla clb a) where
  fmap f ResamplingBuffer {put, get, buffer} =
    ResamplingBuffer
      { put
      , get = fmap (fmap (fmap f)) <$> get
      , buffer
      }
