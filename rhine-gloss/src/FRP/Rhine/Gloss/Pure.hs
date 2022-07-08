{- | A pure @gloss@ backend for Rhine.

To run pure Rhine apps with @gloss@,
write a clocked signal function ('ClSF') in the 'GlossClock' and use 'flowGloss'.
-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module FRP.Rhine.Gloss.Pure
  ( GlossM
  , paint
  , clear
  , paintAll
  , GlossClock (..)
  , GlossClSF
  , currentEvent
  , flowGloss
  , flowGlossClSF
  ) where

-- base
import qualified Control.Category as Category

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Strict

-- dunai
import qualified Control.Monad.Trans.MSF.Reader as MSFReader
import qualified Control.Monad.Trans.MSF.Writer as MSFWriter
import Data.MonadicStreamFunction.InternalCore

-- rhine
import FRP.Rhine

-- rhine-gloss
import FRP.Rhine.Gloss.Common

-- monad-schedule
import Control.Monad.Schedule.Class
import Control.Monad.Schedule.Yield
import Control.Monad.Trans.MSF (performOnFirstSample)
import Data.Functor.Identity

-- * @gloss@ effects

-- FIXME How about a Reader (MSF () (Either Float Event))? That might unify the two backends and make the pure one more flexible.

-- | A pure monad in which all effects caused by the @gloss@ backend take place.
newtype GlossM a = GlossM { unGlossM :: YieldT (ReaderT (Float, Maybe Event) (Writer Picture)) a }
  deriving (Functor, Applicative, Monad)
  -- deriving (Functor, Applicative, Monad, MonadSchedule)

-- FIXME for some reasons deriving gets thrown off by the newtype
instance MonadSchedule GlossM where
  schedule actions = fmap (fmap (fmap GlossM)) $ GlossM $ schedule $ fmap unGlossM actions

-- -- A fake schedule instance that will never be called because the Gloss backend does the scheduling.
-- instance MonadSchedule GlossM where
--   schedule = error "GlossM.schedule should never be called"

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

-- | The overall clock of a pure @rhine@ 'ClSF' that can be run by @gloss@.
--   It ticks both on events (@tag = Just Event@) and simulation steps (@tag = Nothing@).
data GlossClock = GlossClock

instance Semigroup GlossClock where
  _ <> _ = GlossClock

instance Clock GlossM GlossClock where
  type Time GlossClock = Float
  type Tag  GlossClock = Maybe Event
  initClock _ = return (constM (GlossM $ yield >> lift ask) >>> (sumS *** Category.id), 0)

instance GetClockProxy GlossClock

-- * Signal functions

{- |
The type of a 'ClSF' you can implement to get a @gloss@ app,
if you chose to handle events and simulation steps in the same subsystem.

You can, but don't need to paint via 'GlossM':
You can also simply output the picture and it will be painted on top.
-}
type GlossClSF = ClSF GlossM GlossClock () Picture

-- | Observe whether there was an event this tick,
--   and which one.
currentEvent :: ClSF GlossM GlossClock () (Maybe Event)
currentEvent = tagS

-- * Reactimation

-- | Specialisation of 'flowGloss' to a 'GlossClSF'
flowGlossClSF
  :: GlossSettings
  -> GlossClSF -- ^ The @gloss@-compatible 'ClSF'.
  -> IO ()
flowGlossClSF settings clsf = flowGloss settings $ clsf >-> arrMCl paintAll @@ GlossClock

type WorldMSF = MSF Identity ((Float, Maybe Event), ()) (Picture, Maybe ())

-- | The main function that will start the @gloss@ backend and run the 'Rhine'
flowGloss
  :: (Clock GlossM cl, GetClockProxy cl)
  => GlossSettings
  -> Rhine GlossM cl () ()
  -> IO ()
flowGloss GlossSettings { .. } rhine
  = play display backgroundColor stepsPerSecond (worldMSF, Blank) getPic handleEvent simStep
    where

      worldMSF :: WorldMSF
      worldMSF = MSFWriter.runWriterS $ MSFReader.runReaderS $ morphS (runYieldT . unGlossM) $ performOnFirstSample $ eraseClock rhine
      stepWith :: (Float, Maybe Event) -> (WorldMSF, Picture) -> (WorldMSF, Picture)
      stepWith (diff, eventMaybe) (msf, _) = let ((picture, _), msf') = runIdentity $ unMSF msf ((diff, eventMaybe), ()) in (msf', picture)
      getPic (_, pic) = pic
      handleEvent event = stepWith (0, Just event)
      simStep diff = stepWith (diff, Nothing)
