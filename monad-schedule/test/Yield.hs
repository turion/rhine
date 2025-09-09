{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Yield where

-- base
import Control.Monad (forever)
import Data.Foldable (forM_)
import Data.List.NonEmpty (NonEmpty, reverse)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromJust, isJust, maybeToList)

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer (Writer, execWriter, runWriter, tell)

-- QuickCheck
import Test.QuickCheck (counterexample, (==>))

-- test-framework
import Test.Framework

-- test-framework-hunit
import Test.Framework.Providers.HUnit

-- test-framework-quickcheck2
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- HUnit
import Test.HUnit hiding (Test)

-- monad-schedule
import Control.Monad.Schedule.Class (schedule, scheduleAndFinish)
import Control.Monad.Schedule.Trans (runScheduleT)
import Control.Monad.Schedule.Yield

sampleActions :: NonEmpty (MySchedule ())
sampleActions = [yield, yield]

tests =
  testGroup
    "Trans"
    [ testCase "Only leftover time is waited" $
        assertRunsLike sampleActions [Yielded]
    , testCase "Scheduling two waits" $
        assertRunsEqual sampleActions (Data.List.NonEmpty.reverse sampleActions)
    , testCase "Different number of waits" $
        assertRunsLike
          [ myLog "Thread 1 starts" >> yield >> myLog "Thread 1 action" >> yield >> myLog "Thread 1 done"
          , myLog "Thread 2 starts" >> yield >> myLog "Thread 2 done"
          ]
          [ Log "Thread 1 starts"
          , Log "Thread 2 starts"
          , Yielded
          , Log "Thread 1 action"
          , Log "Thread 2 done"
          , Yielded
          , Log "Thread 1 done"
          ]
    , testCase "Blocking thread doesn't starve other thread"
        $ assertRunContains
          [ forever $ myLog "Busy loop starts" >> yield >> myLog "Busy loop ends"
          , myLog "One off thread starts" >> yield >> myLog "One off thread does a thing" >> yield >> myLog "One off thread done"
          ]
        $ Log "One off thread done"
    , testCase "Programs with continuations can be scheduled"
        $ assertProgramsInitiallyRunsLike
          [[Log "Thread 1 active", Log "Thread 2 active", Yielded], [Log "Thread 1 active", Log "Thread 2 active", Yielded], [Log "Thread 1 active", Log "Thread 2 active", Yielded], [Log "Thread 1 active", Log "Thread 2 active", Yielded], [Log "Thread 1 active", Log "Thread 2 active", Yielded]]
          [foreverP (const ["Thread 1 active"]), foreverP (const ["Thread 2 active"])]
        $ repeat True
    , testCase "Two programs that tick alternately can be scheduled" $
        assertProgramsInitiallyRunsLike
          [[Log "1 Nope", Log "2 Yes", Yielded, Log "1 Nope"], [Log "2 Nope", Yielded, Log "2 Nope", Log "1 Yes", Yielded, Log "2 Nope"], [Log "1 Yes", Yielded, Log "2 Nope"], [Log "1 Nope", Yielded, Log "1 Nope", Log "2 Yes", Yielded, Log "1 Nope"], [Log "2 Yes", Yielded, Log "1 Nope"], [Log "2 Yes", Yielded, Log "1 Nope"], [Log "2 Yes", Yielded, Log "1 Nope"], [Log "2 Nope", Yielded, Log "2 Nope", Log "1 Yes", Yielded, Log "2 Nope"]]
          twoPrograms
          [True, False, False, True, True, True, True, False, False]
    , testProperty "Two programs that tick alternately can be scheduled with arbitrary input" $
        \(inputs :: [Bool]) skip ->
          let log = take (20 * length inputs) $ drop skip $ concat $ runProgramWith inputs $ schedulePrograms twoPrograms
              isContained expectedEntry = expectedEntry `elem` log
           in counterexample (show log) $
                all (`elem` drop skip inputs) ([True, False] :: [Bool]) ==>
                  all isContained ([Log "1 Yes", Log "2 Yes"] :: [Event])
    ]

assertRunsEqual :: NonEmpty (MySchedule a1) -> NonEmpty (MySchedule a2) -> Assertion
assertRunsEqual actions1 actions2 = assertEqual "Should run the same under scheduling" (runMySchedule actions1) (runMySchedule actions2)

assertRunsLike :: NonEmpty (MySchedule a) -> [Event] -> Assertion
assertRunsLike actions events = assertEqual "Should run like the following under scheduling" events (runMySchedule actions)

assertRunContains :: NonEmpty (MySchedule a) -> Event -> Assertion
assertRunContains actions event = assertBool ("The run should contain the event " ++ show event) $ event `elem` runMySchedule actions

assertInitiallyRunsLike :: NonEmpty (MySchedule a) -> [Event] -> Assertion
assertInitiallyRunsLike actions events = assertEqual "Should, at the beginning, run like the following under scheduling" events $ take (length events) $ runMySchedule actions

data Event
  = Log String
  | Yielded
  deriving (Eq, Show)

type MySchedule a = YieldT (Writer [Event]) a

myLog :: String -> MySchedule ()
myLog = lift . tell . pure . Log

runMySchedule :: NonEmpty (MySchedule a) -> [Event]
runMySchedule = execWriter . runScheduleT (tell . pure . const Yielded) . scheduleAndFinish

type MyReaderSchedule a = YieldT (ReaderT Bool (Writer [Event])) a

-- Ok this is basically ListT
newtype Program = Program {unProgram :: MyReaderSchedule (Maybe Program)}

foreverP :: (Bool -> [String]) -> Program
foreverP action = go
  where
    go = Program $ do
      input <- lift ask
      lift $ lift $ tell $ Log <$> action input
      yield
      return $ Just go

-- Returning Left means looping further, Right means going to the next continuation
wait :: (Bool -> Either String String) -> Program
wait f = go
  where
    go = Program loop
    loop = do
      input <- lift ask
      case f input of
        Left msg -> do
          myReaderLog msg
          yield
          loop
        Right msg -> do
          myReaderLog msg
          yield
          return $ Just go

twoPrograms =
  [ wait (\b -> if b then Left "1 Nope" else Right "1 Yes")
  , wait (\b -> if b then Right "2 Yes" else Left "2 Nope")
  ]

runProgram :: Program -> MyReaderSchedule ()
runProgram Program {..} = do
  tick <- unProgram
  forM_ tick runProgram

myReaderLog :: String -> MyReaderSchedule ()
myReaderLog = lift . lift . tell . pure . Log

schedulePrograms :: NonEmpty Program -> Program
schedulePrograms programs = Program $ do
  (done, running) <- schedule $ unProgram <$> programs
  return $ fmap schedulePrograms $ NonEmpty.nonEmpty $ (fromJust <$> NonEmpty.filter isJust done) ++ (Program <$> running)

-- Should be possible with some recursion scheme
runProgramWith :: [Bool] -> Program -> [[Event]]
runProgramWith [] _ = []
runProgramWith (input : inputs) Program {..} =
  let (cont, events) = runWriter $ flip runReaderT input $ runScheduleT (const $ lift $ tell [Yielded]) unProgram
   in events : (runProgramWith inputs =<< maybeToList cont)

assertProgramsInitiallyRunsLike :: [[Event]] -> NonEmpty Program -> [Bool] -> Assertion
assertProgramsInitiallyRunsLike events programs inputs =
  assertEqual "The programs, when scheduled, should run like" events $
    take (length events) $
      runProgramWith inputs $
        schedulePrograms programs
