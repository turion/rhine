{- | Example application for the @gloss@ wrapper. -}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Main
  ( module Main
  , module X
  ) where

-- base
import qualified Control.Category as Category
import Control.Concurrent
import Control.Monad (void)
import Data.Data
import Data.Maybe (maybeToList)

-- transformers
import Control.Monad.Trans.Class

-- foreign-store
import Foreign.Store

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.LiveProgram
import LiveCoding.RuntimeIO.Launch

-- rhine
import FRP.Rhine.Reactimation.ClockErasure

-- rhine-gloss
import FRP.Rhine.Gloss as X

-- | Calculate a gear wheel rotated by a certain angle.
gears :: Float -> Picture
gears angle = color red $ pictures
  $ map (rotate angle)
  $
  [ rotate (45 * n) $ rectangleSolid 20 150 | n <- [0..3] ]
  ++
  [ circleSolid 60
  , translate 40 0 $ color green $ circleSolid 10
  , translate 60 0 $ color blue $ circleSolid 10
  ]

-- | Rotate the gear with a constant angular velocity.
--   Disregards all events.
sim :: Monad m => BehaviourF m Float [Event] Picture
sim = proc events -> do
  pic <- arr (const 20) >>> integrate >>> arr gears -< ()
  nEvents <- sumS -< EventLength $ 2 * length events
  returnA -< pic <> scale 0.1 0.1 (text (show nEvents))
  -- returnA -< pic

newtype EventLength a = EventLength a
  deriving (Show, Num, Data)


main :: IO ()
main = do
  putStrLn "Please choose between the pure (1), the pure combined (2), the IO backend (3), the IO backend without events (4), the IO livecoding backend (5):"
  n <- readLn
  case n of
    1 -> flowGloss defaultSettings pureClSF
    2 -> flowGlossCombined defaultSettings pureRhine
    3 -> flowGlossIO defaultSettings ioRhine
    4 -> launchGlossThread defaultSettings $ reactimateCl GlossSimClockIO $ arr (const []) >-> sim >-> arrMCl paintAllIO
    5 -> do
      void $ flowGlossLive defaultSettings ioRhine
      void $ getLine
    _ -> error "Invalid input"

-- | Run the gears simulation with the pure backend synchronously.
pureClSF = currentEvent >>> arr maybeToList >>> sim

-- | Run the gears simulation with the pure backend with two subsystems,
--   one at the rate of events, one at the rate of simulation.
pureRhine = tagS @@ glossEventClock >-- collect -@- glossSchedule --> sim >-> arrMCl paintAll @@ glossSimulationClock


type LiveClock = SequentialClock (GlossConcT IO) GlossEventClockIO GlossSimClockIO

-- | Run the gears simulation with the 'IO' backend.
ioRhine :: Rhine (GlossConcT IO) LiveClock () ()
ioRhine = tagS @@ GlossEventClockIO >-- collect -@- glossConcurrently --> sim >-> arrMCl paintAllIO @@ GlossSimClockIO

load :: IO (RhineHandle LiveClock, GlossEnv)
load = readStore (Store 0)

save :: (RhineHandle LiveClock, GlossEnv) -> IO ()
save varVars = writeStore (Store 0) varVars

launchGlossRhine
  :: ( Clock (GlossConcT IO) cl
     , GetClockProxy cl
     , Time cl ~ Time (In  cl)
     , Time cl ~ Time (Out cl)
     )
  => Rhine (GlossConcT IO) cl () () -> GlossEnv -> IO (RhineHandle cl)
launchGlossRhine Rhine {..} vars = do
  (runningClock, initTime) <- runGlossConcT (initClock clock) vars
  clockVar <- newEmptyMVar
  void $ forkIO $ reactimate $ morphS (flip runGlossConcT vars) runningClock >>> arrM (putMVar clockVar)
  runningCell <- launch $ liveCell $ morphS (flip runGlossConcT vars) $ eraseClockRunningAndSN (constM (lift $ takeMVar clockVar)) initTime sn
  return RhineHandle { .. }

updateGlossRhine
  :: ( Clock (GlossConcT IO) cl
     , GetClockProxy cl
     , Time cl ~ Time (In  cl)
     , Time cl ~ Time (Out cl)
     )
  => RhineHandle cl -> Rhine (GlossConcT IO) cl () () -> GlossEnv -> IO ()
updateGlossRhine RhineHandle {..} Rhine {..} vars = do
  update runningCell $ liveCell $ morphS (flip runGlossConcT vars) $ eraseClockRunningAndSN (constM (lift $ takeMVar clockVar)) initTime sn


liveinit _ = return $ unlines
  [ "vars <- launchGlossThread defaultSettings"
  , "handle <- launchGlossRhine ioRhine vars"
  -- , "var <- launch $ liveCell $ morphS (flip runGlossConcT vars) cell"
  , "save (handle, vars)"
  ]

livereload _ = return $ unlines
  [ ":reload"
  , "(handle, vars) <- load"
  -- , "cell <- runGlossConcT (eraseClockRhine ioRhine) vars"
  , "updateGlossRhine handle ioRhine vars"
  ]
