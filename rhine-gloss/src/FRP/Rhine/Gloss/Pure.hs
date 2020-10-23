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
  , flowGlossWithWorldMSF
  ) where

-- base
import qualified Control.Category as Category

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

-- dunai
import qualified Control.Monad.Trans.MSF.Reader as MSFReader
import Data.MonadicStreamFunction.InternalCore

-- rhine
import FRP.Rhine
import FRP.Rhine.Reactimation.ClockErasure

-- rhine-gloss
import FRP.Rhine.Gloss.Common
import Control.Monad.Schedule.Class

-- * @gloss@ effects

-- FIXME How about a Reader (MSF () (Either Float Event))? That might unify the two backends and make the pure one more flexible.

-- | A pure monad in which all effects caused by the @gloss@ backend take place.
newtype GlossM a = GlossM { unGlossM :: (ReaderT (Float, Maybe Event)) (Writer Picture) a }
  deriving (Functor, Applicative, Monad)

-- A fake schedule instance that will never be called because the Gloss backend does the scheduling.
instance MonadSchedule GlossM where
  schedule = error "GlossM.schedule should never be called"

-- | Add a picture to the canvas.
paint :: Picture -> GlossM ()
paint = GlossM . lift . tell

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
  initClock _ = return (constM (GlossM ask) >>> (sumS *** Category.id), 0)

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

-- | The main function that will start the @gloss@ backend and run the 'SN'
--   (in the case of the combined clock).
flowGloss
  :: GlossSettings
  -> GlossClSF -- ^ The @gloss@-compatible 'Rhine'.
  -> IO ()
flowGloss settings clsf = flowGlossWithWorldMSF settings GlossClock $ proc (time, tag) -> do
  arrM (const clear) -< ()
  pic <- eraseClockClSF getClockProxy 0 clsf -< (time, tag, ())
  arrM paint -< pic


-- FIXME Hide?
-- | Helper function
flowGlossWithWorldMSF
  :: Clock GlossM cl
  => GlossSettings
  -> cl
  -> MSF GlossM (Time cl, Tag cl) b
  -> IO ()
flowGlossWithWorldMSF GlossSettings { .. } clock msf
  = play display backgroundColor stepsPerSecond (worldMSF, Blank) getPic handleEvent simStep
    where
      worldMSF = MSFReader.runReaderS $ morphS unGlossM $ proc () -> do
        (time, tag) <- fst $ fst $ runWriter $ flip runReaderT (0, Nothing) $ unGlossM $ initClock clock -< ()
        msf -< (time, tag)
      getPic (_, pic) = pic
      stepWith (diff, maybeEvent) (msf, _) = snd *** id $ runWriter $ unMSF msf ((diff, maybeEvent), ())
      handleEvent event = stepWith (0, Just event)
      simStep diff = stepWith (diff, Nothing)
