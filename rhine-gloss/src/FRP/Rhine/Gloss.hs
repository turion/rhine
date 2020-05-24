{- | Wrapper library to write Gloss applications in Rhine.
@gloss@ acts as the backend here.

A Rhine app with the Gloss backend must use the 'GlossClock',
since the @gloss@ API only offers callbacks.
In order to run such a reactive program, you have to use 'flowGloss'.
-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module FRP.Rhine.Gloss
  ( module FRP.Rhine.Gloss
  , module X
  )
  where

-- base
import Data.Maybe (fromMaybe)
import Data.Functor.Identity (Identity, runIdentity)

import qualified Control.Arrow as X

-- gloss
import Graphics.Gloss.Interface.Pure.Game

import qualified Graphics.Gloss as X

-- dunai
import Data.MonadicStreamFunction.InternalCore

-- rhine
import FRP.Rhine
import FRP.Rhine.Reactimation.ClockErasure

import qualified FRP.Rhine      as X
import qualified FRP.Rhine.ClSF as X

-- rhine-gloss
import FRP.Rhine.Gloss.Internals

-- TODO Consider generalising to IO


-- | The overall clock of a valid @rhine@ 'SN' that can be run by @gloss@.
--   @a@ is the type of subevents that are selected.
type GlossClock a
  = SequentialClock Identity
      (SelectClock GlossEventClock a)
      GlossSimulationClock_

-- | The type of a valid 'Rhine' that can be run by @gloss@.
--   @a@ is the type of subevents that are selected.
type GlossRhine a = Rhine Identity (GlossClock a) () Picture

-- | The type of a 'ClSF' that you have to implement to get a @gloss@ app.
type GlossClSF a = ClSF Identity GlossSimulationClock [a] Picture

{- | For most applications, it is sufficient to implement
a single signal function
that is called with a list of all relevant events
that occurred in the last tick.
-}
buildGlossRhine
  :: (Event -> Maybe a) -- ^ The event selector
  -> GlossClSF a        -- ^ The 'ClSF' representing the game loop.
  -> GlossRhine a
buildGlossRhine select clsfSim
  =   timeInfoOf tag @@  SelectClock { mainClock = GlossEventClock, .. }
  >-- collect -@- glossSchedule
  --> withProperSimClock clsfSim @@ GlossSimulationClock_

-- | The main function that will start the @gloss@ backend and run the 'SN'.
flowGloss
  :: Display      -- ^ Display mode (e.g. 'InWindow' or 'FullScreen').
  -> Color        -- ^ Background color.
  -> Int          -- ^ Number of simulation steps per second of real time.
  -> GlossRhine a -- ^ The @gloss@-compatible 'Rhine'.
  -> IO ()
flowGloss display color n Rhine {..}
  = play display color n worldMSF getPic handleEvent simStep
  where
    worldMSF = feedback Blank $ proc (input, lastPic) -> do
      case input of
        Just usual -> do
          maybeNewPic <- eraseClockSN () sn -< usual
          let newPic = fromMaybe lastPic maybeNewPic
          returnA -< (newPic, newPic)
        Nothing -> returnA -< (lastPic, lastPic)
    getPic msf = fst $ runIdentity $ unMSF msf Nothing
    handleEvent event msf = case select (sequentialCl1 clock) event of
      Just a  -> snd $ runIdentity $ unMSF msf $ Just ((), Left a, Just ()) -- Event is relevant
      Nothing -> msf -- Event is irrelevant, state doesn't change
    simStep diff msf = snd $ runIdentity $ unMSF msf $ Just ((), Right diff, Nothing)
