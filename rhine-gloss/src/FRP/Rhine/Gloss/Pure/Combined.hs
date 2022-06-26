{- | A pure @gloss@ backend for Rhine,
with separated event and simulation loop.

To run pure Rhine apps with @gloss@,
write a signal network ('SN') in the 'GlossCombinedClock' and use 'flowGlossCombined'.
As an easy starter, you can use the helper function 'buildGlossRhine'.
-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}

module FRP.Rhine.Gloss.Pure.Combined where

-- rhine
import FRP.Rhine
import FRP.Rhine.Reactimation.ClockErasure

-- rhine-gloss
import FRP.Rhine.Gloss.Common
import FRP.Rhine.Gloss.Pure

-- | The overall clock of a pure @rhine@ 'SN' that can be run by @gloss@.
--   It is combined of two subsystems, the event part and the simulation part.
--   @a@ is the type of subevents that are selected.
type GlossCombinedClock a
  = SequentialClock GlossM
      (GlossEventClock a)
      GlossSimulationClock

-- | Schedule the subclocks of the 'GlossCombinedClock'.
glossSchedule :: Schedule GlossM (GlossEventClock a) GlossSimulationClock
glossSchedule = schedSelectClocks

-- ** Events

-- | The clock that ticks whenever a specific @gloss@ event occurs.
type GlossEventClock a = SelectClock GlossClock a

-- | Select the relevant events by converting them to @Just a@,
--   and the irrelevant ones to 'Nothing'.
glossEventSelectClock
  :: (Event -> Maybe a)
  -> GlossEventClock a
glossEventSelectClock selector = SelectClock
  { mainClock = GlossClock
  , select = (>>= selector)
  }

-- | Tick on every event.
glossEventClock :: GlossEventClock Event
glossEventClock = glossEventSelectClock Just

-- ** Simulation

-- | The clock that ticks for every @gloss@ simulation step.
type GlossSimulationClock = SelectClock GlossClock ()

glossSimulationClock :: GlossSimulationClock
glossSimulationClock = SelectClock { .. }
  where
    mainClock = GlossClock
    select (Just _event) = Nothing
    select Nothing        = Just ()

-- * Signal networks

{- |
The type of a valid 'Rhine' that can be run by @gloss@,
if you chose to separate events and simulation into two subsystems.
@a@ is the type of subevents that are selected.

All painting has to be done in 'GlossM', e.g. via the 'paint' method.

Typically, such a 'Rhine' is built something like this:

@
-- | Select only key press events
myEventClock :: GlossEventClock Key
myEventClock = glossEventSelectClock selector
  where
    selector (EventKey key _ _ _) = Just key
    selector _ = Nothing

myEventSubsystem :: ClSF GlossM GlossEventClock () MyType
myEventSubsystem = ...

mySim :: ClSF GlossM GlossSimulationClock [MyType] ()
mySim = ...

myGlossRhine :: GlossRhine a
myGlossRhine
  = myEventSubsystem @@ myEventClock >-- collect -@- glossSchedule --> mySim @@ glossSimulationClock
@
-}
type GlossRhine a = Rhine GlossM (GlossCombinedClock a) () ()

{- | For most applications, it is sufficient to implement
a single signal function
that is called with a list of all relevant events
that occurred in the last tick.
-}
buildGlossRhine
  :: (Event -> Maybe a) -- ^ The event selector
  -> ClSF GlossM GlossSimulationClock [a] () -- ^ The 'ClSF' representing the game loop.
  -> GlossRhine a
buildGlossRhine selector clsfSim
  =   timeInfoOf tag @@ glossEventSelectClock selector
  >-- collect       -@- glossSchedule
  --> clsfSim        @@ glossSimulationClock

-- * Reactimation

-- | The main function that will start the @gloss@ backend and run the 'SN'.
flowGlossCombined
  :: GlossSettings
  -> GlossRhine a -- ^ The @gloss@-compatible 'Rhine'.
  -> IO ()
flowGlossCombined settings Rhine { .. } = flowGlossWithWorldMSF settings clock $ proc tick -> do
  eraseClockSN 0 sn -< case tick of
    (_       , Left event) -> (0       , Left event, Just ())
    (diffTime, Right ()  ) -> (diffTime, Right ()  , Nothing)
