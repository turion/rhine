{- | Wrapper library to write Gloss applications in Rhine.
@gloss@ acts as the backend here.

A Rhine app with the Gloss backend must use the 'GlossClock',
since the @gloss@ API only offers callbacks.
In order to run such a reactive program, you have to use 'flowGloss'.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module FRP.Rhine.Gloss
  ( module FRP.Rhine.Gloss
  , module X
  , GlossSimWithEventClock
  )
  where

-- base
import Data.Functor.Identity (Identity, runIdentity)

import qualified Control.Arrow as X

-- gloss
import Graphics.Gloss.Interface.Pure.Game

import qualified Graphics.Gloss as X

-- rhine
import FRP.Rhine hiding (trivialResamplingBuffer)
import FRP.Rhine.Reactimation.Tick

import qualified FRP.Rhine      as X
import qualified FRP.Rhine.ClSF as X

-- rhine-gloss
import FRP.Rhine.Gloss.Internals

-- TODO Consider generalising to IO

-- * Pure @gloss@ clocks

-- | The overall clock of a valid @rhine@ 'SN' that can be run by @gloss@.
--   @a@ is the type of subevents that are selected.
type GlossClock a
  = SequentialClock Identity
      (SelectClock GlossEventClock a)
      GlossSimulationClock_


-- * Pure signal processing components to use with @gloss@ clocks.

-- | The type of a valid 'Rhine' that can be run by @gloss@.
--   @event@ is the type of subevents that are selected.
type GlossRhine event = Rhine Identity (GlossClock event) () Picture

-- | Like 'GlossRhine', but for 'GlossSimWithEventClock_'.
type GlossSimWithEventRhine event = Rhine Identity (GlossSimWithEventClock_ event) () Picture


-- | The type of a 'ClSF' that you have to implement to get a @gloss@ app.
type GlossClSF event = ClSF Identity GlossSimulationClock [event] Picture

-- | Like 'GlossClSF', but for 'GlossSimWithEventClock'.
--   The event data has to be retrieved from the 'Tag'.
type GlossSimWithEventClSF event = ClSF Identity (GlossSimWithEventClock event) () Picture

-- * Utilities to build @rhine-gloss@ components with default buffers and schedules

{- | For most applications, it is sufficient to implement
a single signal function
that is called with a list of all relevant events
that occurred in the last tick.
-}
buildGlossRhine
  :: (Event -> Maybe event) -- ^ The event selector
  -> GlossClSF event        -- ^ The 'ClSF' representing the game loop.
  -> GlossRhine event
buildGlossRhine select clsfSim
  =   timeInfoOf tag @@  SelectClock { mainClock = GlossEventClock, .. }
  >-- collect -@- glossSchedule
  --> withProperSimClock clsfSim @@ GlossSimulationClock_

-- | Like 'buildGlossRhine', but for 'GlossSimWithEventClock'.
buildGlossSimWithEventRhine
  :: (Event -> Maybe event)      -- ^ The event selector
  -> GlossSimWithEventClSF event -- ^ The 'ClSF' representing the game loop.
  -> GlossSimWithEventRhine event
buildGlossSimWithEventRhine select clsfSim
  = withProperSimEventClock clsfSim @@ glossSimWithEventClock_ select

-- * Main loops


-- | The main function that will start the @gloss@ backend and run the 'SN'.
flowGloss
  :: Display          -- ^ Display mode (e.g. 'InWindow' or 'FullScreen').
  -> Color            -- ^ Background color.
  -> Int              -- ^ Number of simulation steps per second of real time.
  -> GlossRhine event -- ^ The @gloss@-compatible 'Rhine'.
  -> IO ()
flowGloss display color n Rhine {..}
  = play display color n world getPic handleEvent simStep
  where
    graphicsBuffer
      :: ResamplingBuffer Identity
           GlossSimulationClock_ GlossGraphicsClock
           Picture               Picture
    graphicsBuffer = keepLast Blank
    world = createTickable (trivialResamplingBuffer clock) sn graphicsBuffer ()
    getPic Tickable { buffer2 } = fst $ runIdentity $ get buffer2 $ TimeInfo () () () ()
    handleEvent event tickable = case select (sequentialCl1 clock) event of
      Just a  -> runIdentity $ tick tickable () $ Left a -- Event is relevant
      Nothing -> tickable -- Event is irrelevant, state doesn't change
    simStep diff tickable = runIdentity $ tick tickable () $ Right diff


-- | Like 'flowGloss', but for 'GlossSimWithEventClock'.
flowGlossSimWithEvent
  :: Display      -- ^ Display mode (e.g. 'InWindow' or 'FullScreen').
  -> Color        -- ^ Background color.
  -> Int          -- ^ Number of simulation steps per second of real time.
  -> GlossSimWithEventRhine event -- ^ The @gloss@-compatible 'Rhine'.
  -> IO ()
flowGlossSimWithEvent display color n Rhine {..}
  = play display color n world getPic handleEvent simStep
  where
    graphicsBuffer
      :: ResamplingBuffer Identity
           (GlossSimWithEventClock_ event) GlossGraphicsClock
           Picture                         Picture
    graphicsBuffer = keepLast Blank
    world = createTickable (trivialResamplingBuffer clock) sn graphicsBuffer ()
    getPic Tickable { buffer2 } = fst $ runIdentity $ get buffer2 $ TimeInfo () () () ()
    handleEvent event tickable = case select (parallelCl1 clock) event of
      Just a  -> runIdentity $ tick tickable () $ Left a -- Event is relevant
      Nothing -> tickable -- Event is irrelevant, state doesn't change
    simStep diff tickable = runIdentity $ tick tickable () $ Right diff
