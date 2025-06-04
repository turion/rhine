{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- | A pure @gloss@ backend for Rhine.

To run pure Rhine apps with @gloss@,
write a clocked signal function ('ClSF') in the 'GlossClock' and use 'flowGloss'.
-}
module FRP.Rhine.Gloss.Pure (
  GlossM,
  paint,
  clear,
  paintAll,
  GlossClock (..),
  GlossClSF,
  currentEvent,
  flowGloss,
  flowGlossClSF,
) where

-- base
import qualified Control.Category as Category
import Data.Functor.Identity

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Strict

-- monad-schedule
import Control.Monad.Schedule.Class
import Control.Monad.Schedule.Yield

-- automaton
import qualified Data.Automaton.Trans.Reader as AutomatonReader
import qualified Data.Automaton.Trans.Writer as AutomatonWriter

-- rhine
import FRP.Rhine

-- rhine-gloss
import FRP.Rhine.Gloss.Common

-- * @gloss@ effects

-- | A pure monad in which all effects caused by the @gloss@ backend take place.
newtype GlossM a = GlossM {unGlossM :: YieldT (ReaderT (Float, Maybe Event) (Writer Picture)) a}
  deriving (Functor, Applicative, Monad)

-- Would have liked to make this a derived instance, but for some reason deriving gets thrown off by the newtype
instance MonadSchedule GlossM where
  schedule actions = fmap (fmap (fmap GlossM)) $ GlossM $ schedule $ fmap unGlossM actions

-- | Add a picture to the canvas.
paint :: Picture -> GlossM ()
paint = GlossM . lift . lift . tell

-- FIXME This doesn't what you think it does

-- | Clear the canvas.
clear :: GlossM ()
clear = paint Blank

-- | Clear the canvas and then paint.
paintAll :: Picture -> GlossM ()
paintAll pic = clear >> paint pic

-- * Clocks

{- | The overall clock of a pure @rhine@ 'ClSF' that can be run by @gloss@.
   It ticks both on events (@tag = Just Event@) and simulation steps (@tag = Nothing@).
-}
data GlossClock = GlossClock

instance Semigroup GlossClock where
  _ <> _ = GlossClock

instance Clock GlossM GlossClock where
  type Time GlossClock = Float
  type Tag GlossClock = Maybe Event
  initClock _ = return (constM (GlossM $ yield >> lift ask) >>> (sumS *** Category.id), 0)
  {-# INLINE initClock #-}

instance GetClockProxy GlossClock

-- * Signal functions

{- |
The type of a 'ClSF' you can implement to get a @gloss@ app,
if you chose to handle events and simulation steps in the same subsystem.

You can, but don't need to paint via 'GlossM':
You can also simply output the picture and it will be painted on top.
-}
type GlossClSF = ClSF GlossM GlossClock () Picture

{- | Observe whether there was an event this tick,
   and which one.
-}
currentEvent :: ClSF GlossM GlossClock () (Maybe Event)
currentEvent = tagS

-- * Reactimation

-- | Specialisation of 'flowGloss' to a 'GlossClSF'
flowGlossClSF ::
  GlossSettings ->
  -- | The @gloss@-compatible 'ClSF'.
  GlossClSF ->
  IO ()
flowGlossClSF settings clsf = flowGloss settings $ Present ^>>@ (clsf >-> arrMCl paintAll @@ GlossClock) @>>^ const ()

type WorldAutomaton = Automaton Identity ((Float, Maybe Event), ()) (Picture, ())

-- | The main function that will start the @gloss@ backend and run the 'Rhine'
flowGloss ::
  GlossSettings ->
  Rhine GlossM td cls () () ->
  IO ()
flowGloss GlossSettings {..} rhine =
  play display backgroundColor stepsPerSecond (worldAutomaton, Blank) getPic handleEvent simStep
  where
    worldAutomaton :: WorldAutomaton
    worldAutomaton = AutomatonWriter.runWriterS $ AutomatonReader.runReaderS $ hoistS (runYieldT . unGlossM) $ eraseClock rhine
    stepWith :: (Float, Maybe Event) -> (WorldAutomaton, Picture) -> (WorldAutomaton, Picture)
    stepWith (diff, eventMaybe) (automaton, _) = let Result automaton' (picture, _) = runIdentity $ stepAutomaton automaton ((diff, eventMaybe), ()) in (automaton', picture)
    getPic (_, pic) = pic
    handleEvent event = stepWith (0, Just event)
    simStep diff = stepWith (diff, Nothing)
