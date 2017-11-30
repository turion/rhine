{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{- | Wrapper library to write Gloss applications in Rhine.
@gloss@ acts as the backend here.

A Rhine app with the Gloss backend must use the 'GlossClock',
since the @gloss@ API only offers callbacks.
In order to run such a reactive program, you have to use 'flowGloss'.
-}
module FRP.Rhine.Gloss where

-- base
import Control.Category (id)
import Data.Functor.Identity (Identity, runIdentity)
import Prelude hiding (id)

-- gloss
import Graphics.Gloss.Interface.Pure.Game

-- dunai
import Control.Monad.Trans.MSF.Reader (readerS, runReaderS)

-- rhine
import FRP.Rhine
import FRP.Rhine.Clock.Select
import FRP.Rhine.Reactimation.Tick
import FRP.Rhine.ResamplingBuffer.Collect
import FRP.Rhine.ResamplingBuffer.KeepLast

-- TODO Consider generalising to IO

-- | The error message that gets thrown when you try to start a @gloss@ app with 'flow'.
errMsg :: String
errMsg =  "You cannot start gloss apps with FRP.Rhine.flow. "
       ++ "Use FRP.Rhine.Gloss.flowGloss instead."

-- | The clock that ticks whenever a @gloss@ event occurs.
data GlossEventClock = GlossEventClock

instance Clock m GlossEventClock where
  type TimeDomainOf GlossEventClock = ()
  type Tag          GlossEventClock = Event
  startClock _ = error errMsg

-- | The clock that ticks for every @gloss@ simulation step,
--   but only shows the time delta in the tag.
--   Usually, you don't need this clock, but rather 'GlossSimulationClock'.
data GlossSimulationClock_ = GlossSimulationClock_

instance Clock m GlossSimulationClock_ where
  type TimeDomainOf GlossSimulationClock_ = ()
  type Tag          GlossSimulationClock_ = Float
  startClock _ = error errMsg

-- | The clock that ticks for every @gloss@ simulation step.
--   Use 'withProperSimClock' to transform to 'GlossSimulationClock_'.
data GlossSimulationClock = GlossSimulationClock

instance Clock m GlossSimulationClock where
  type TimeDomainOf GlossSimulationClock = Float
  type Tag          GlossSimulationClock = ()
  startClock _ = error errMsg

-- | To use all features of the 'SyncSF' framework,
--   write your synchronous stream function on the 'GlossSimulationClock'
--   and then use this function to transform it.
withProperSimClock
  :: Monad m
  => SyncSF m GlossSimulationClock  a b
  -> SyncSF m GlossSimulationClock_ a b
withProperSimClock syncsf = readerS $ (intermingle *** id) >>> runReaderS syncsf
  where
    intermingle :: Monad m => MSF m (TimeInfo GlossSimulationClock_) (TimeInfo GlossSimulationClock)
    intermingle = proc TimeInfo {tag} -> do
      let sinceTick = tag
      absolute <- sumS -< sinceTick
      let sinceStart = absolute
      returnA          -< TimeInfo { tag = (), .. }

-- | The clock that ticks for every @gloss@ graphics output.
data GlossGraphicsClock = GlossGraphicsClock

instance Clock m GlossGraphicsClock where
  type TimeDomainOf GlossGraphicsClock = ()
  type Tag          GlossGraphicsClock = ()
  startClock _ = error errMsg


-- | The overall clock of a valid @rhine@ 'SF' that can be run by @gloss@.
--   @a@ is the type of subevents that are selected.
type GlossClock a
  = SequentialClock Identity
      (SelectClock GlossEventClock a)
      GlossSimulationClock_

-- | A schedule you can't actually use, for internal purposes.
glossSchedule :: Schedule Identity (SelectClock GlossEventClock a) GlossSimulationClock_
glossSchedule = error errMsg

-- | The type of a valid 'Rhine' that can be run by @gloss@.
--   @a@ is the type of subevents that are selected.
type GlossRhine a = Rhine Identity (GlossClock a) () Picture

type GlossSyncSF a = SyncSF Identity GlossSimulationClock [a] Picture

{- | For most applications, it is sufficient to implement
a single synchronous signal function
that is called with a list of all relevant events
that occurred in the last tick.
-}
buildGlossRhine
  :: (Event -> Maybe a) -- ^ The event selector
  -> GlossSyncSF a      -- ^ The 'SyncSF'
  -> GlossRhine a
buildGlossRhine select syncsfSim
  =   timeInfoOf tag @@  SelectClock { mainClock = GlossEventClock, .. }
  >-- collect -@- glossSchedule
  --> withProperSimClock syncsfSim @@ GlossSimulationClock_

-- | The main function that will start the @gloss@ backend and run the 'SF'.
flowGloss
  :: Display      -- ^ Display mode (e.g. 'InWindow' or 'FullScreen').
  -> Color        -- ^ Background color.
  -> Int          -- ^ Number of simulation steps per second of real time.
  -> GlossRhine a -- ^ The @gloss@-compatible 'Rhine'.
  -> IO ()
flowGloss display color n Rhine {..}
  = play display color n world getPic handleEvent simStep
  where
    graphicsBuffer
      :: ResamplingBuffer Identity
           GlossSimulationClock_ GlossGraphicsClock
           Picture               Picture
    graphicsBuffer = keepLast Blank
    world = createTickable (trivialResamplingBuffer clock) sf graphicsBuffer ()
    getPic Tickable { buffer2 } = fst $ runIdentity $ get buffer2 $ TimeInfo () () () ()
    handleEvent event tickable = case select (sequentialCl1 clock) event of
      Just a  -> runIdentity $ tick tickable () $ Left a -- Event is relevant
      Nothing -> tickable -- Event is irrelevant, state doesn't change
    simStep diff tickable = runIdentity $ tick tickable () $ Right diff
