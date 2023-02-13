{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module FRP.Rhine.Reactimation.Live where

-- base
import Control.Concurrent
import Control.Monad (void)

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.RuntimeIO.Launch

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.Reactimation.ClockErasure
import FRP.Rhine.Schedule
import FRP.Rhine.Type
import LiveCoding (launch, LaunchedProgram)

data RhineHandle cl = RhineHandle
  { runningCell :: LaunchedProgram IO
  , clockVar :: MVar (Time cl, Tag cl)
  , initTime :: Time cl
  }

launchRhine
  :: ( Clock IO cl
     , GetClockProxy cl
     , Time cl ~ Time (In  cl)
     , Time cl ~ Time (Out cl)
     )
  => Rhine IO cl () () -> IO (RhineHandle cl)
launchRhine Rhine {..} = do
  (runningClock, initTime) <- initClock clock
  clockVar <- newEmptyMVar
  void $ forkIO $ reactimate $ runningClock >>> arrM (putMVar clockVar)
  runningCell <- launch $ liveCell $ eraseClockRunningAndSN (constM (takeMVar clockVar)) initTime sn
  return RhineHandle { .. }

updateRhine
  :: ( Clock IO cl
     , GetClockProxy cl
     , Time cl ~ Time (In  cl)
     , Time cl ~ Time (Out cl)
     )
  => RhineHandle cl -> Rhine IO cl () () -> IO ()
updateRhine RhineHandle {..} Rhine {..} = --update _ _
  update runningCell $ liveCell $ eraseClockRunningAndSN (constM (takeMVar clockVar)) initTime sn
