-- | A simple example that creates an event every second
--   and handles it by outputting it on the console.
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
-- base
import Control.Concurrent
import Control.Monad (void)
import Data.Maybe (listToMaybe)
import Text.Printf
import Text.Read (readMaybe)

-- random
import System.Random


-- rhine
import FRP.Rhine

-- * Subsystems

-- | The 'IO' monad,
--   augmented with the capability of sending and receiving events.
type EventIO = EventChanT String IO

-- ** Sample data

-- | Output "Hello World!" every second.
message :: Monad m => ClSF m (HoistClock IO EventIO (Millisecond 1000)) () String
message = arr $ const "Hello World!"

-- | Perform a random computation, using a lot of CPU time.
randomNumbers :: MonadIO m => Behaviour m time String
randomNumbers = constMCl $ liftIO $ do
  m <- randomRIO (4, 6 :: Integer)
  x <- randomRIO (-3, 3 :: Double)
  return $ "Random number: " ++ printf "%.3f" (iterate tan x !! (10 ^ m))

-- | Each time an event arrives, this function is called.
--   It simply outputs the event on the console.
handleEvents :: (MonadIO m, Tag cl ~ String) => ClSF m cl () ()
handleEvents = tagS >>> arrMCl (putStrLn >>> liftIO)

-- * Running the subsystems in the same thread, or in separate threads

-- | Run both subsystems in parallel, on the same thread.
--   (Only scheduling happens concurrently.)
eventExample :: IO ()
eventExample = runEventChanT $ flow
  $ emitEventSystem |@| handleEventSystem
  where
    emitEventSystem   = message >-> emitS @@ liftClock waitClock
    handleEventSystem = handleEvents      @@ EventClock

-- | Run both subsystems in different threads.
--   This means that the channel must be created manually.
threadsExample :: IO ()
threadsExample = do
  chan <- newChan
  void $ forkIO $ flow $ handleEvents      @@ (eventClockOn chan :: HoistClock EventIO IO (EventClock String))
  withChan chan $ flow $ message >-> emitS @@ liftClock waitClock


-- | For the following examples, we introduce a third system
--   that tries to run in parallel at a higher frequency,
--   but will be caused to lag because it is run in the same thread
--   as a computationally expensive one.
responsive :: ClSF IO (Millisecond 100) () ()
responsive = timeInfo >>> proc TimeInfo {..} -> do
  arrMCl putStrLn -< "Current time: " ++ show sinceInit
  arrMCl putStrLn -< "Real time " ++ (if tag then "" else "UN") ++ "successful"


-- | Producing random numbers using a lot of CPU causes all subsystems on the same thread to lag.
randomsExample :: IO ()
randomsExample = runEventChanT $ flow wholeSystem
  where
    busy = (liftClock Busy :: HoistClock IO EventIO Busy) -- TODO Can't remove brackets. GHC parser bug?
    emitEventSystem   = randomNumbers >-> emitS @@ busy
    handleEventSystem = handleEvents            @@ EventClock
    eventSystem = emitEventSystem |@| handleEventSystem
    responsiveSystem = liftClSFAndClock responsive @@ liftClock waitClock -- TODO This can be lifted in one go
    wholeSystem = eventSystem |@| responsiveSystem


-- | Put the expensive computation on a separate thread,
--   and evaluate it there completely, using 'emitS'', before sending it back.
--   This resolves the lag.
threadsRandomsExample :: IO ()
threadsRandomsExample = do
  chan <- newChan
  let
    eventClock = eventClockOn chan :: HoistClock EventIO IO (EventClock String)
    eventSystem = handleEvents @@ eventClock |@| responsive @@ waitClock
    busy = (liftClock Busy :: HoistClock IO EventIO Busy) -- TODO Can't remove brackets. GHC parser bug?
  void $ forkIO $ flow $ eventSystem
  withChan chan $ flow $ randomNumbers >-> emitS' @@ busy


-- * The main program

main :: IO ()
main = do
  putStrLn "Enter a number and press Return to run:"
  putStrLn "1) eventExample"
  putStrLn "2) threadsExample"
  putStrLn "3) randomsExample"
  putStrLn "4) threadsRandomsExample"
  s <- getLine
  let
    mExample = do
      n <- readMaybe s
      examples `atSafe` n
      where
        examples = [ eventExample
                   , threadsExample
                   , randomsExample
                   , threadsRandomsExample
                   ]
        atSafe as n = listToMaybe $ drop (n-1) as
  case mExample of
    Nothing      -> do
      putStrLn "Invalid input."
      main
    Just example -> example
