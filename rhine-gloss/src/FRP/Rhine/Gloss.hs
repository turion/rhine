{- | Wrapper library to write Gloss applications in Rhine.
@gloss@ acts as the backend here.

A Rhine app with the Gloss backend must use the 'GlossClock',
since the @gloss@ API only offers callbacks.
In order to run such a reactive program, you have to use 'flowGloss'.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeFamilies     #-}
module FRP.Rhine.Gloss
  ( module FRP.Rhine.Gloss
  , module X
  )
  where

-- base
import Data.Functor.Identity (Identity, runIdentity)

import qualified Control.Arrow as X

-- gloss
import Graphics.Gloss.Interface.Pure.Game

import qualified Graphics.Gloss as X

-- rhine
import FRP.Rhine
import FRP.Rhine.Clock.Select
import FRP.Rhine.Reactimation.Tick
import FRP.Rhine.ResamplingBuffer.Collect
import FRP.Rhine.ResamplingBuffer.KeepLast

import qualified FRP.Rhine        as X
import qualified FRP.Rhine.SyncSF as X

-- rhine-gloss
import FRP.Rhine.Gloss.Internals

-- TODO Consider generalising to IO


-- | The overall clock of a valid @rhine@ 'SF' that can be run by @gloss@.
--   @a@ is the type of subevents that are selected.
type GlossClock a
  = SequentialClock Identity
      (SelectClock GlossEventClock a)
      GlossSimulationClock_

-- | The type of a valid 'Rhine' that can be run by @gloss@.
--   @a@ is the type of subevents that are selected.
type GlossRhine a = Rhine Identity (GlossClock a) () Picture

-- | The type of a 'SyncSF' that you have to implement to get a @gloss@ app.
type GlossSyncSF a = SyncSF Identity GlossSimulationClock [a] Picture

{- | For most applications, it is sufficient to implement
a single synchronous signal function
that is called with a list of all relevant events
that occurred in the last tick.
-}
buildGlossRhine
  :: (Event -> Maybe a) -- ^ The event selector
  -> GlossSyncSF a      -- ^ The 'SyncSF' representing the game loop.
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
