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
  launchGlossThread,
  flowGlossIO,
  glossConcurrently,
)
where

-- base
import Control.Concurrent
import Data.Functor (void)
import Data.IORef

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

-- mmorph
import Control.Monad.Morph

-- gloss
import Graphics.Gloss.Interface.IO.Game

-- rhine
import FRP.Rhine

-- rhine-gloss
import FRP.Rhine.Gloss.Common

-- * Gloss effects

data GlossEnv = GlossEnv
  { timeVar :: MVar Float
  , eventVar :: MVar Event
  , picRef :: IORef Picture
  , time :: Float
  }

-- | Wraps the concurrent variables needed for communication with the @gloss@ backend.
newtype GlossConcT m a = GlossConcT
  {unGlossConcT :: ReaderT GlossEnv m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MFunctor, MMonad)

withPicRef ::
  MonadIO m =>
  (IORef Picture -> IO a) ->
  GlossConcT m a
withPicRef action = GlossConcT $ do
  GlossEnv {picRef} <- ask
  liftIO $ action picRef

-- | Add a picture to the canvas.
paintIO :: MonadIO m => Picture -> GlossConcT m ()
paintIO pic = withPicRef $ \ref -> modifyIORef' ref (<> pic)

-- | Clear the canvas.
clearIO :: MonadIO m => GlossConcT m ()
clearIO = withPicRef $ \ref -> writeIORef ref Blank

-- | Clear the canvas and then paint.
paintAllIO :: MonadIO m => Picture -> GlossConcT m ()
paintAllIO pic = clearIO >> paintIO pic

-- * Gloss clocks in 'IO'

-- | Concurrently block on @gloss@ events.
data GlossEventClockIO = GlossEventClockIO

instance MonadIO m => Clock (GlossConcT m) GlossEventClockIO where
  type Time GlossEventClockIO = Float
  type Tag GlossEventClockIO = Event
  initClock _ = return (constM getEvent, 0)
    where
      getEvent = do
        GlossEnv {eventVar, time} <- GlossConcT ask
        liftIO $ do
          event <- takeMVar eventVar
          return (time, event)

instance GetClockProxy GlossEventClockIO

-- | Concurrently block on @gloss@ simulation ticks.
data GlossSimClockIO = GlossSimClockIO

instance MonadIO m => Clock (GlossConcT m) GlossSimClockIO where
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
  MonadIO m =>
  GlossSettings ->
  GlossConcT m a ->
  m a
launchGlossThread GlossSettings {..} glossLoop = do
  vars <- liftIO $ GlossEnv <$> newEmptyMVar <*> newEmptyMVar <*> newIORef Blank <*> pure 0
  let
    getPic GlossEnv {picRef} = readIORef picRef
    -- Only try to put so this doesn't hang in case noone is listening for events or ticks
    handleEvent event vars@GlossEnv {eventVar} = do
      void $ tryPutMVar eventVar event
      return vars
    simStep diffTime vars@GlossEnv {timeVar, time} = do
      let !time' = time + diffTime
      void $ tryPutMVar timeVar time'
      return vars {time = time'}
  void $ liftIO $ forkIO $ playIO display backgroundColor stepsPerSecond vars getPic handleEvent simStep
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
flowGlossIO settings = launchGlossThread settings . flow

{- | A schedule in the 'GlossConcT' transformer,
   supplying the same backend connection to its scheduled clocks.
-}
glossConcurrently ::
  ( Clock (GlossConcT IO) cl1
  , Clock (GlossConcT IO) cl2
  , Time cl1 ~ Time cl2
  ) =>
  Schedule (GlossConcT IO) cl1 cl2
glossConcurrently = Schedule $
  \cl1 cl2 -> GlossConcT $
    ReaderT $
      \vars ->
        first liftTransS
          <$> initSchedule
            concurrently
            (runGlossEnvClock vars cl1)
            (runGlossEnvClock vars cl2)

type RunGlossEnvClock cl = HoistClock (GlossConcT IO) IO cl

runGlossEnvClock ::
  GlossEnv ->
  cl ->
  RunGlossEnvClock cl
runGlossEnvClock env unhoistedClock =
  HoistClock
    { monadMorphism = flip runReaderT env . unGlossConcT
    , ..
    }

-- FIXME And a schedule for gloss clocks and other clocks
