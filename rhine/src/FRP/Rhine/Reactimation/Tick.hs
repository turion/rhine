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
import FRP.Rhine.SN


{- | A signal network ('SN') enclosed by matching 'ResamplingBuffer's and further auxiliary data,
such that it can be stepped with each arriving tick from a clock 'cl'.
They play a similar role like 'ReactHandle's in dunai.

The type parameters:

* 'm': The monad in which the 'SN' and the 'ResamplingBuffer's produce side effects
* 'cla': The (irrelevant) input clock of the left 'ResamplingBuffer'
* 'clb': The clock at which the left 'ResamplingBuffer' produces output
* 'cl': The clock at which the 'SN' ticks
* 'clc': The clock at which the right 'ResamplingBuffer' accepts input
* 'cld': The (irrelevant) output clock of the right 'ResamplingBuffer'
* 'a': The (irrelevant) input type of the left 'ResamplingBuffer'
* 'b': The input type of the 'SN'
* 'c': The output type of the 'SN'
* 'd': The (irrelevant) output type of the right 'ResamplingBuffer'
-}
data Tickable m cla clb cl clc cld a b c d = Tickable
  { -- | The left buffer from which the input is taken.
    buffer1     :: ResamplingBuffer m cla clb          a b
    -- | The signal network that will process the data.
  , ticksn      :: SN               m        cl          b c
    -- | The right buffer in which the output is stored.
  , buffer2     :: ResamplingBuffer m          clc cld     c d
    -- | The leftmost clock of the signal network, 'cl',
    --   may be a parallel subclock of the buffer clock.
    --   'parClockIn' specifies in which position 'In cl'
    --   is a parallel subclock of 'clb'.
  , parClockIn  :: ParClockInclusion (In  cl) clb
    -- | The same on the output side.
  , parClockOut :: ParClockInclusion (Out cl) clc
    -- | The last times when the different parts of the signal tree have ticked.
  , lastTime    :: LastTime cl
    -- | The time when the whole clock was initialised.
  , initTime    :: Time cl
  }


-- | Initialise the tree of last tick times.
initLastTime :: SN m cl a b -> Time cl -> LastTime cl
initLastTime (Synchronous _)        initTime = LeafLastTime initTime
initLastTime (Sequential sn1 _ sn2) initTime =
  SequentialLastTime
    (initLastTime sn1 initTime)
    (initLastTime sn2 initTime)
initLastTime (Parallel sn1 sn2)     initTime =
  ParallelLastTime
    (initLastTime sn1 initTime)
    (initLastTime sn2 initTime)

-- | Initialise a 'Tickable' from a signal network,
--   two matching enclosing resampling buffers and an initial time.
createTickable
  :: ResamplingBuffer m cla (In cl)                 a b
  -> SN               m         cl                      b c
  -> ResamplingBuffer m                (Out cl) cld     c d
  -> Time cl
  -> Tickable         m cla (In cl) cl (Out cl) cld a b c d
createTickable buffer1 ticksn buffer2 initTime = Tickable
  { parClockIn  = ParClockRefl
  , parClockOut = ParClockRefl
  , lastTime    = initLastTime ticksn initTime
  , ..
  }

{- | In this function, one tick, or step of an asynchronous signal network happens.
The 'TimeInfo' holds the information which part of the signal tree will tick.
This information is encoded in the 'Tag' of the 'TimeInfo',
which is of type 'Either tag1 tag2' in case of a 'SequentialClock' or a 'ParallelClock',
encoding either a tick for the left clock or the right clock.
-}
tick :: ( Monad m, Clock m cl
        , Time cla ~ Time cl
        , Time clb ~ Time cl
        , Time clc ~ Time cl
        , Time cld ~ Time cl
        , Time (In  cl) ~ Time cl
        , Time (Out cl) ~ Time cl
        )
     => Tickable    m cla clb cl clc cld a b c d
     -> Time cl -- ^ Timestamp of the present tick
     -> Tag cl -- ^ 'Tag' of the overall clock; contains the information which subsystem will become active
     -> m (Tickable m cla clb cl clc cld a b c d)
-- Only if we have reached a leaf of the tree, data is actually processed.
tick Tickable
  { ticksn   = Synchronous clsf
  , lastTime = LeafLastTime lastTime
  , .. } now tag = do
    let
      ti = TimeInfo
        { sinceLast = diffTime now lastTime
        , sinceInit = diffTime now initTime
        , absolute  = now
        , tag       = tag
        }
    -- Get an input value from the left buffer
    (b, buffer1') <- get buffer1 $ retag (parClockTagInclusion parClockIn ) ti
    -- Run it through the signal function
    (c, clsf')  <- unMSF clsf b `runReaderT` ti
    -- Put the output into the right buffer
    buffer2'      <- put buffer2 (retag (parClockTagInclusion parClockOut) ti) c
    return Tickable
      { buffer1  = buffer1'
      , ticksn   = Synchronous clsf'
      , buffer2  = buffer2'
      , lastTime = LeafLastTime now
      , .. }
-- The left part of a sequential composition is stepped.
tick tickable@Tickable
  { ticksn   = Sequential sn1 bufferMiddle sn2
  , lastTime = SequentialLastTime lastTimeL lastTimeR
  , initTime
  , parClockIn
  } now (Left tag) = do
    leftTickable <- tick Tickable
      { buffer1     = buffer1 tickable
      , ticksn      = sn1
      , buffer2     = bufferMiddle
      , parClockIn  = parClockIn
      , parClockOut = ParClockRefl
      , lastTime    = lastTimeL
      , initTime    = initTime
      } now tag
    return $ tickable
      { buffer1  = buffer1 leftTickable
      , ticksn   = Sequential (ticksn leftTickable) (buffer2 leftTickable) sn2
      , lastTime = SequentialLastTime (lastTime leftTickable) lastTimeR
      }
-- The right part of a sequential composition is stepped.
tick tickable@Tickable
  { ticksn   = Sequential sn1 bufferMiddle sn2
  , lastTime = SequentialLastTime lastTimeL lastTimeR
  , initTime
  , parClockOut
  } now (Right tag) = do
    rightTickable <- tick Tickable
      { buffer1     = bufferMiddle
      , ticksn      = sn2
      , buffer2     = buffer2 tickable
      , parClockIn  = ParClockRefl
      , parClockOut = parClockOut
      , lastTime    = lastTimeR
      , initTime    = initTime
      } now tag
    return $ tickable
      { buffer2  = buffer2 rightTickable
      , ticksn   = Sequential sn1 (buffer1 rightTickable) (ticksn rightTickable)
      , lastTime = SequentialLastTime lastTimeL (lastTime rightTickable)
      }
-- A parallel composition is stepped.
tick tickable@Tickable
  { ticksn   = Parallel snA snB
  , lastTime = ParallelLastTime lastTimeA lastTimeB
  , initTime
  , parClockIn
  , parClockOut
  } now tag = case tag of
    Left tagL -> do
      leftTickable <- tick Tickable
        { buffer1     = buffer1 tickable
        , ticksn      = snA
        , buffer2     = buffer2 tickable
        , parClockIn  = ParClockInL parClockIn
        , parClockOut = ParClockInL parClockOut
        , lastTime    = lastTimeA
        , initTime    = initTime
        } now tagL
      return $ tickable
        { buffer1  = buffer1 leftTickable
        , ticksn   = Parallel (ticksn leftTickable) snB
        , buffer2  = buffer2 leftTickable
        , lastTime = ParallelLastTime (lastTime leftTickable) lastTimeB
        }
    Right tagR -> do
      rightTickable <- tick Tickable
        { buffer1     = buffer1 tickable
        , ticksn      = snB
        , buffer2     = buffer2 tickable
        , parClockIn  = ParClockInR parClockIn
        , parClockOut = ParClockInR parClockOut
        , lastTime    = lastTimeB
        , initTime    = initTime
        } now tagR
      return $ tickable
        { buffer1  = buffer1 rightTickable
        , ticksn   = Parallel snA (ticksn rightTickable)
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
  -> ResamplingBuffer m (Out cl) (In cl) () ()
trivialResamplingBuffer _ = go
  where
    go  = ResamplingBuffer {..}
    put _ _ = return      go
    get _   = return ((), go)
