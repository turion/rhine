{- | This module contains internals needed for the reactimation of signal functions.
None of it should be relevant for a typical user of this library.
-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module FRP.Rhine.Reactimation.Tick where

-- transformers
import Control.Monad.Trans.Reader

-- dunai
import Data.MonadicStreamFunction

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.Schedule
import FRP.Rhine.SF


{- | A signal function ('SF') enclosed by matching 'ResamplingBuffer's and further auxiliary data,
such that it can be stepped with each arriving tick from a clock 'cl'.
They play a similar role like 'ReactHandle's in dunai.

The type parameters:

* 'm': The monad in which the 'SF' and the 'ResamplingBuffer's produce side effects
* 'cla': The (irrelevant) input clock of the left 'ResamplingBuffer'
* 'clb': The clock at which the left 'ResamplingBuffer' produces output
* 'cl': The clock at which the 'SF' ticks
* 'clc': The clock at which the right 'ResamplingBuffer' accepts input
* 'cld': The (irrelevant) output clock of the right 'ResamplingBuffer'
* 'a': The (irrelevant) input type of the left 'ResamplingBuffer'
* 'b': The input type of the 'SF'
* 'c': The output type of the 'SF'
* 'd': The (irrelevant) output type of the right 'ResamplingBuffer'
-}
data Tickable m cla clb cl clc cld a b c d = Tickable
  { -- | The left buffer from which the input is taken.
    buffer1     :: ResamplingBuffer m cla clb          a b
    -- | The signal function that will process the data.
  , ticksf      :: SF               m        cl          b c
    -- | The right buffer in which the output is stored.
  , buffer2     :: ResamplingBuffer m          clc cld     c d
    -- | The leftmost clock of the signal function, 'cl',
    --   may be a parallel subclock of the buffer clock.
    --   'parClockInL' specifies in which position 'Leftmost cl'
    --   is a parallel subclock of 'clb'.
  , parClockInL :: ParClockInclusion (Leftmost  cl) clb
    -- | The same on the output side.
  , parClockInR :: ParClockInclusion (Rightmost cl) clc
    -- | The last times when the different parts of the signal tree have ticked.
  , lastTime    :: LastTime cl
    -- | The time when the whole clock was initialised.
  , initTime    :: TimeDomainOf cl
  }


-- | Initialise the tree of last tick times.
initLastTime :: SF m cl a b -> TimeDomainOf cl -> LastTime cl
initLastTime (Synchronous _)        initTime = LeafLastTime initTime
initLastTime (Sequential sf1 _ sf2) initTime =
  SequentialLastTime
    (initLastTime sf1 initTime)
    (initLastTime sf2 initTime)
initLastTime (Parallel sf1 sf2)     initTime =
  ParallelLastTime
    (initLastTime sf1 initTime)
    (initLastTime sf2 initTime)

-- | Initialise a 'Tickable' from a signal function,
--   two matching enclosing resampling buffers and an initial time.
createTickable
  :: ResamplingBuffer m cla (Leftmost cl)                       a b
  -> SF               m                   cl                      b c
  -> ResamplingBuffer m                      (Rightmost cl) cld     c d
  -> TimeDomainOf cl
  -> Tickable         m cla (Leftmost cl) cl (Rightmost cl) cld a b c d
createTickable buffer1 ticksf buffer2 initTime = Tickable
  { parClockInL = ParClockRefl
  , parClockInR = ParClockRefl
  , lastTime    = initLastTime ticksf initTime
  , ..
  }

{- | In this function, one tick, or step of an asynchronous signal function happens.
The 'TimeInfo' holds the information which part of the signal tree will tick.
This information is encoded in the 'Tag' of the 'TimeInfo',
which is of type 'Either tag1 tag2' in case of a 'SequentialClock' or a 'ParallelClock',
encoding either a tick for the left clock or the right clock.
-}
tick :: ( Monad m, Clock m cl
        , TimeDomainOf cla ~ TimeDomainOf cl
        , TimeDomainOf clb ~ TimeDomainOf cl
        , TimeDomainOf clc ~ TimeDomainOf cl
        , TimeDomainOf cld ~ TimeDomainOf cl
        , TimeDomainOf (Leftmost  cl) ~ TimeDomainOf cl
        , TimeDomainOf (Rightmost cl) ~ TimeDomainOf cl
        )
     => Tickable    m cla clb cl clc cld a b c d
     -> TimeDomainOf cl -- ^ Timestamp of the present tick
     -> Tag cl -- ^ 'Tag' of the overall clock; contains the information which subsystem will become active
     -> m (Tickable m cla clb cl clc cld a b c d)
-- Only if we have reached a leaf of the tree, data is actually processed.
tick Tickable
  { ticksf   = Synchronous syncsf
  , lastTime = LeafLastTime lastTime
  , .. } now tag = do
    let
      ti = TimeInfo
        { sinceTick  = diffTime now lastTime
        , sinceStart = diffTime now initTime
        , absolute   = now
        , tag        = tag
        }
    -- Get an input value from the left buffer
    (b, buffer1') <- get buffer1 $ retag (parClockTagInclusion parClockInL) ti
    -- Run it through the synchronous signal function
    (c, syncsf')  <- unMSF syncsf b `runReaderT` ti
    -- Put the output into the right buffer
    buffer2'      <- put buffer2 (retag (parClockTagInclusion parClockInR) ti) c
    return Tickable
      { buffer1  = buffer1'
      , ticksf   = Synchronous syncsf'
      , buffer2  = buffer2'
      , lastTime = LeafLastTime now
      , .. }
-- The left part of a sequential composition is stepped.
tick tickable@Tickable
  { ticksf   = Sequential sf1 bufferMiddle sf2
  , lastTime = SequentialLastTime lastTimeL lastTimeR
  , initTime
  , parClockInL
  } now (Left tag) = do
    leftTickable <- tick Tickable
      { buffer1     = buffer1 tickable
      , ticksf      = sf1
      , buffer2     = bufferMiddle
      , parClockInL = parClockInL
      , parClockInR = ParClockRefl
      , lastTime    = lastTimeL
      , initTime    = initTime
      } now tag
    return $ tickable
      { buffer1  = buffer1 leftTickable
      , ticksf   = Sequential (ticksf leftTickable) (buffer2 leftTickable) sf2
      , lastTime = SequentialLastTime (lastTime leftTickable) lastTimeR
      }
-- The right part of a sequential composition is stepped.
tick tickable@Tickable
  { ticksf   = Sequential sf1 bufferMiddle sf2
  , lastTime = SequentialLastTime lastTimeL lastTimeR
  , initTime
  , parClockInR
  } now (Right tag) = do
    rightTickable <- tick Tickable
      { buffer1     = bufferMiddle
      , ticksf      = sf2
      , buffer2     = buffer2 tickable
      , parClockInL = ParClockRefl
      , parClockInR = parClockInR
      , lastTime    = lastTimeR
      , initTime    = initTime
      } now tag
    return $ tickable
      { buffer2  = buffer2 rightTickable
      , ticksf   = Sequential sf1 (buffer1 rightTickable) (ticksf rightTickable)
      , lastTime = SequentialLastTime lastTimeL (lastTime rightTickable)
      }
-- A parallel composition is stepped.
tick tickable@Tickable
  { ticksf   = Parallel sfA sfB
  , lastTime = ParallelLastTime lastTimeA lastTimeB
  , initTime
  , parClockInL
  , parClockInR
  } now tag = case tag of
    Left tagL -> do
      leftTickable <- tick Tickable
        { buffer1     = buffer1 tickable
        , ticksf      = sfA
        , buffer2     = buffer2 tickable
        , parClockInL = ParClockInL parClockInL
        , parClockInR = ParClockInL parClockInR
        , lastTime    = lastTimeA
        , initTime    = initTime
        } now tagL
      return $ tickable
        { buffer1  = buffer1 leftTickable
        , ticksf   = Parallel (ticksf leftTickable) sfB
        , buffer2  = buffer2 leftTickable
        , lastTime = ParallelLastTime (lastTime leftTickable) lastTimeB
        }
    Right tagR -> do
      rightTickable <- tick Tickable
        { buffer1     = buffer1 tickable
        , ticksf      = sfB
        , buffer2     = buffer2 tickable
        , parClockInL = ParClockInR parClockInL
        , parClockInR = ParClockInR parClockInR
        , lastTime    = lastTimeB
        , initTime    = initTime
        } now tagR
      return $ tickable
        { buffer1  = buffer1 rightTickable
        , ticksf   = Parallel sfA (ticksf rightTickable)
        , buffer2  = buffer2 rightTickable
        , lastTime = ParallelLastTime lastTimeA (lastTime rightTickable)
        }
tick Tickable {} _ _ = error "Impossible pattern in tick"

-- TODO It seems wasteful to unwrap and rewrap log(N) Tickables
-- (where N is the size of the clock tree) each tick,
-- but I have no better idea.

{- | A 'ResamplingBuffer' producing only units.
(Slightly more efficient and direct implementation than the one in 'FRP.Rhine.Timeless'
that additionally unifies the clock types in a way needed for the tick implementation.)
-}
trivialResamplingBuffer
  :: Monad m => cl
  -> ResamplingBuffer m (Rightmost cl) (Leftmost cl) () ()
trivialResamplingBuffer _ = go
  where
    go  = ResamplingBuffer {..}
    put _ _ = return      go
    get _   = return ((), go)
