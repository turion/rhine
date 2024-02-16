{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Wrapper to write @gloss@ applications in Rhine, using concurrency.
module FRP.Rhine.Gloss.IO (
  GlossConcT,
  paintIO,
  clearIO,
  paintAllIO,
  GlossEventClockIO (..),
  GlossSimClockIO (..),
  launchInGlossThread,
  launchGlossThread,
  flowGlossIO,
  runGlossEnvClock,
  RunGlossEnvClock,
)
where

-- base
import Control.Concurrent
import Control.Monad (when)
import Data.Functor (void)
import Data.IORef
import GHC.Float (float2Double)

-- time
import Data.Time.Clock

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

-- mmorph
import Control.Monad.Morph

-- gloss
import Graphics.Gloss.Interface.IO.Game

-- monad-schedule
import Control.Monad.Schedule.Class

-- rhine
import FRP.Rhine

-- rhine-gloss
import FRP.Rhine.Gloss.Common

-- * Gloss effects

data GlossEnv = GlossEnv
  { timeVar :: MVar Float
  , eventVar :: MVar Event
  , picRef :: IORef Picture
  , timeRef :: IORef Float
  }

-- | Wraps the concurrent variables needed for communication with the @gloss@ backend.
newtype GlossConcT m a = GlossConcT
  {unGlossConcT :: ReaderT GlossEnv m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MFunctor, MMonad)

instance (Monad m, MonadSchedule m) => MonadSchedule (GlossConcT m) where
  schedule actions = GlossConcT $ fmap (second $ map GlossConcT) $ schedule $ unGlossConcT <$> actions

withPicRef ::
  (MonadIO m) =>
  (IORef Picture -> IO a) ->
  GlossConcT m a
withPicRef action = GlossConcT $ do
  GlossEnv {picRef} <- ask
  liftIO $ action picRef

-- | Add a picture to the canvas.
paintIO :: (MonadIO m) => Picture -> GlossConcT m ()
paintIO pic = withPicRef $ \ref -> modifyIORef' ref (<> pic)

-- | Clear the canvas.
clearIO :: (MonadIO m) => GlossConcT m ()
clearIO = withPicRef $ \ref -> writeIORef ref Blank

-- | Clear the canvas and then paint.
paintAllIO :: (MonadIO m) => Picture -> GlossConcT m ()
paintAllIO pic = clearIO >> paintIO pic

-- * Gloss clocks in 'IO'

-- | Concurrently block on @gloss@ events.
data GlossEventClockIO = GlossEventClockIO

instance (MonadIO m) => Clock (GlossConcT m) GlossEventClockIO where
  type Time GlossEventClockIO = Float
  type Tag GlossEventClockIO = Event
  initClock _ = return (constM getEvent, 0)
    where
      getEvent = do
        GlossEnv {eventVar, timeRef} <- GlossConcT ask
        liftIO $ do
          event <- takeMVar eventVar
          time <- readIORef timeRef
          return (time, event)

instance GetClockProxy GlossEventClockIO

-- | Concurrently block on @gloss@ simulation ticks.
data GlossSimClockIO = GlossSimClockIO

instance (MonadIO m) => Clock (GlossConcT m) GlossSimClockIO where
  type Time GlossSimClockIO = Float
  type Tag GlossSimClockIO = ()
  initClock _ = return (constM getTime &&& arr (const ()), 0)
    where
      getTime = do
        GlossEnv {timeVar} <- GlossConcT ask
        liftIO $ takeMVar timeVar

instance GetClockProxy GlossSimClockIO

-- * Reactimation

{- | Apply this to supply the 'GlossConcT' effect.
   Creates a new thread in which @gloss@ is run,
   and feeds the clocks 'GlossEventClockIO' and 'GlossSimClockIO'.

   Usually, this function is applied to the result of 'flow',
   so you can handle all occurring effects as needed.
   If you only use @gloss@ in your whole signal network,
   you can use 'flowGlossIO' instead.
-}
launchGlossThread ::
  (MonadIO m) =>
  GlossSettings ->
  m GlossEnv
launchGlossThread GlossSettings {..} = do
  vars <- liftIO $ GlossEnv <$> newEmptyMVar <*> newEmptyMVar <*> newIORef Blank <*> newIORef 0
  let
    getPic GlossEnv {picRef} = readIORef picRef
    -- Only try to put so this doesn't hang in case noone is listening for events or ticks
    handleEvent event vars@GlossEnv {eventVar} = do
      void $ tryPutMVar eventVar event
      return vars
    simStep diffTime vars@GlossEnv {timeVar, timeRef} = do
      time <- readIORef timeRef
      let !time' = time + diffTime
      timeUpdate <- tryPutMVar timeVar time'
      when timeUpdate $ writeIORef timeRef time'
      return vars
  void $ liftIO $ forkIO $ playIO display backgroundColor stepsPerSecond vars getPic handleEvent simStep
  return vars

{- | Apply this to supply the 'GlossConcT' effect.
   Creates a new thread in which @gloss@ is run,
   and feeds the clocks 'GlossEventClockIO' and 'GlossSimClockIO'.

   Usually, this function is applied to the result of 'flow',
   so you can handle all occurring effects as needed.
   If you only use @gloss@ in your whole signal network,
   you can use 'flowGlossIO' instead.
-}
launchInGlossThread ::
  (MonadIO m) =>
  GlossSettings ->
  GlossConcT m a ->
  m a
launchInGlossThread settings glossLoop = do
  vars <- launchGlossThread settings
  runReaderT (unGlossConcT glossLoop) vars

{- | Run a 'Rhine' in the 'GlossConcT' monad by launching a separate thread for the @gloss@ backend,
   and reactimate in the foreground.
-}
flowGlossIO ::
  ( MonadIO m
  , Clock (GlossConcT m) cl
  , GetClockProxy cl
  , Time cl ~ Time (In cl)
  , Time cl ~ Time (Out cl)
  ) =>
  GlossSettings ->
  Rhine (GlossConcT m) cl () () ->
  m ()
flowGlossIO settings = launchInGlossThread settings . flow

{- | Apply this wrapper to your clock type @cl@ in order to escape the 'GlossConcT' transformer.
  The resulting clock will be in @m@, not 'GlossConcT m' anymore.
  Typically, @m@ will have the 'MonadIO' constraint.
-}
type RunGlossEnvClock m cl = HoistClock (GlossConcT m) m cl

{- | Apply to a gloss clock to remove a 'GlossConcT' layer.
  You will have to have initialized a 'GlossEnv', for example by calling 'launchGlossThread'.
-}
runGlossEnvClock ::
  GlossEnv ->
  cl ->
  RunGlossEnvClock m cl
runGlossEnvClock env unhoistedClock =
  HoistClock
    { monadMorphism = flip runReaderT env . unGlossConcT
    , ..
    }

-- * Rescaled clocks in other time domains

-- | Rescale a 'GlossSimClockIO' to the 'Double' time domain
type GlossSimClockIODouble = RescaledClock GlossSimClockIO Double

glossSimClockIODouble :: GlossSimClockIODouble
glossSimClockIODouble =
  RescaledClock
    { unscaledClock = GlossSimClockIO
    , rescale = float2Double
    }

{- | Rescale the gloss clocks so they will be compatible with real 'UTCTime'.

This is needed for compatibility with other realtime clocks like 'Millisecond'.
-}
type GlossClockUTC cl = RescaledClockS (GlossConcT IO) cl UTCTime (Tag cl)

-- | Rescale a gloss clock like 'GlossSimClockIO' or 'GlossEventClockIO' to the UTC time domain.
glossClockUTC :: cl -> GlossClockUTC cl
glossClockUTC cl =
  RescaledClockS
    { unscaledClockS = cl
    , rescaleS = const $ do
        now <- liftIO getCurrentTime
        return (arrM $ \(_timePassed, event) -> (,event) <$> liftIO getCurrentTime, now)
    }
