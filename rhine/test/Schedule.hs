{-# LANGUAGE OverloadedLists #-}

module Schedule where

-- base
import Control.Category (id)
import Control.Monad (replicateM_)
import Control.Monad.IO.Class
import Data.Char (toUpper)
import Data.Functor (($>))
import Data.Functor.Identity
import Prelude hiding (id)

-- transformers
import Control.Monad.Trans.Reader

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit

-- FIXME could rewrite everthing with MonadConc and then do Dejafu
-- Or else io-sim
-- monad-schedule
import Control.Monad.Schedule.Trans (Schedule, runScheduleT, wait)

-- time
import Data.Time.Clock

-- rhine
import Control.Concurrent (forkIO, killThread, myThreadId, newEmptyMVar, putMVar, readChan, takeMVar, threadDelay, writeChan, yield)
import FRP.Rhine
import Util

tests =
  testGroup
    "Schedule"
    [ testGroup
        "scheduleList"
        [ testCase "schedule waits chronologically" $ do
            let output = runIdentity $ runScheduleT (const (pure ())) $ embed (scheduleList $ (\n -> constM (wait n $> n) >>> accumulateWith (+) 0) <$> [3 :: Integer, 5]) $ replicate 6 ()
            output @?= pure <$> [3, 5, 6, 9, 10, 12]
        , testCase "schedule waits chronologically (mirrored)" $ do
            let output = runSchedule $ embed (scheduleList $ (\n -> constM (wait n $> n) >>> accumulateWith (+) 0) <$> [5 :: Integer, 3]) $ replicate 6 ()
            output @?= pure <$> [3, 5, 6, 9, 10, 12]
        ]
    , testGroup
        "runningSchedule"
        [ testCase "chronological ticks" $ do
            let clA = FixedStep @5
                clB = FixedStep @3
                (runningClockA, _) = runSchedule (initClock clA :: RunningClockInit (Schedule Integer) Integer ())
                (runningClockB, _) = runSchedule (initClock clB :: RunningClockInit (Schedule Integer) Integer ())
                output = runSchedule $ embed (runningSchedule clA clB runningClockA runningClockB) $ replicate 6 ()
            output
              @?= [ (3, Right ())
                  , (5, Left ())
                  , (6, Right ())
                  , (9, Right ())
                  , (10, Left ())
                  , (12, Right ())
                  ]
        ]
    , testGroup
        "ParallelClock"
        [ testCase "chronological ticks" $ do
            let
              (runningClock, _time) = runSchedule (initClock $ ParallelClock (FixedStep @5) (FixedStep @3) :: RunningClockInit (Schedule Integer) Integer (Either () ()))
              output = runSchedule $ embed runningClock $ replicate 6 ()
            output
              @?= [ (3, Right ())
                  , (5, Left ())
                  , (6, Right ())
                  , (9, Right ())
                  , (10, Left ())
                  , (12, Right ())
                  ]
        ]
    , testGroup
        "schedulePair"
        [ testGroup
            "IO"
            [ testCase "Two threadDelays" $ do
                let component n = constM $ threadDelay n >> return n
                    msf = schedulePair (component 10000) (component 16180)
                result <- embed msf $ replicate 6 ()
                result @?= [10000, 16180, 10000, 10000, 16180, 10000]
            , testCase "MVar producer and consumer" $ do
                var <- newEmptyMVar
                let producer = constM $ threadDelay 100 >> putMVar var () >> return "produced"
                    consumer = constM $ threadDelay 161 >> takeMVar var >> return "consumed"
                    system = schedulePair producer consumer
                result <- embed system $ replicate 6 ()
                result @?= concat (replicate 3 ["consumed", "produced"])
            , testCase "MVar producer and consumer" $ do
                var <- newEmptyMVar
                let producer = constM $ putMVar var () >> return "produced 2"
                    consumer = constM $ takeMVar var >> return "consumed 2"
                    system = schedulePair producer consumer
                result <- embed system $ replicate 6 ()
                result @?= concat (replicate 3 ["consumed", "produced"])
            , testCase "MVar external producer and threadDelay" $ do
                var <- newEmptyMVar
                let consumer = constM $ threadDelay 16100 >> takeMVar var >> return "consumed"
                    regular = constM $ threadDelay 10000 >> return "ms"
                    system = schedulePair consumer regular
                forkIO $ replicateM_ 3 $ putMVar var ()
                result <- embed system $ replicate 6 ()
                result @?= ["ms", "consumed", "ms", "ms", "consumed", "ms"]
            , testCase "MVar external producer and threadDelay, ArrowChoice" $ do
                var <- newEmptyMVar
                let consumer = constM $ threadDelay 16100 >> takeMVar var >> return (Left "consumed")
                    regular = constM $ threadDelay 10000 >> return (Right "ms")
                    system = schedulePair consumer regular >>> (id +++ arr (fmap toUpper))
                forkIO $ replicateM_ 3 $ putMVar var ()
                result <- embed system $ replicate 6 ()
                result @?= [Right "MS", Left "consumed", Right "MS", Right "MS", Left "consumed", Right "MS"]
            , testCase "EventClock-like Chans" $ do
                chan <- newChan
                let consumer = constM $ readChan chan
                    producer = count >>> arrM (writeChan chan) >>> constM (return (-1 :: Int))
                    system = schedulePair consumer producer
                result <- embed system $ replicate 4 ()
                result @?= [-1, -1, 1, 2]
            ]
        , testGroup
            "EventChanT"
            [ testCase "eventExample directly" $ do
                let regular = constM $ do
                      chan <- ask
                      liftIO $ do
                        writeChan chan ()
                        threadDelay 1000
                        yield
                    consumer = constM $ ask >>= liftIO . readChan
                    system = schedulePair consumer regular
                threadId <- liftIO myThreadId
                liftIO $ forkIO $ threadDelay 2000000 >> killThread threadId
                result <- runEventChanT $ embed system $ replicate 4 ()
                result @?= [(), (), (), ()]
            , testCase "eventExample from example" $ do
                result <- runEventChanT $ do
                  let runningEClock = constM $ do
                        chan <- ask
                        liftIO $ putStrLn "reading chan"
                        event <- liftIO $ readChan chan
                        time <- liftIO getCurrentTime
                        return (time, event)
                  (runningWClock, _) <- initClock $ (liftClock waitClock :: (HoistClock IO (EventChanT String IO) (Millisecond 1000)))
                  let runningClock = schedulePair (runningEClock >>> arr (second Left)) (runningWClock >>> arr (second Right))
                  threadId <- liftIO myThreadId
                  liftIO $ forkIO $ threadDelay 2000000 >> killThread threadId
                  embed (runningClock :: MSF (EventChanT String IO) () (UTCTime, Either String Bool)) (replicate 1 ())
                --   return ([] :: [()])
                result @?= []
            ]
        ]
    ]
