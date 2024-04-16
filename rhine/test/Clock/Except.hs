{-# LANGUAGE OverloadedStrings #-}

module Clock.Except where

-- base
import Control.Applicative (Alternative (empty))
import Data.Either (isLeft)
import GHC.IO.Handle (hDuplicateTo)
import System.IO (IOMode (ReadMode), stdin, withFile)
import System.IO.Error (isEOFError)

-- mtl
import Control.Monad.Writer.Class

-- transformers
-- Replace Strict by CPS when bumping mtl to 2.3
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Writer.Strict hiding (tell)

-- text
import Data.Text (Text)

-- tasty
import Test.Tasty (TestTree, testGroup)

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?), (@?=))

-- rhine
import FRP.Rhine
import FRP.Rhine.Clock.Except (
  CatchClock (CatchClock),
  DelayIOError,
  DelayMonadIOError,
  ExceptClock (ExceptClock),
  catchClSF,
  delayIOError,
  delayMonadIOError',
 )
import Paths_rhine

tests :: TestTree
tests =
  testGroup
    "Except"
    [exceptClockTests, catchClockTests, delayedClockTests, innerWriterTests]

-- * 'Except'

type E = ExceptT IOError IO
type EClock = ExceptClock StdinClock IOError

exceptClock :: EClock
exceptClock = ExceptClock StdinClock

exceptClockTests :: TestTree
exceptClockTests =
  testGroup
    "ExceptClock"
    [ testCase "Raises the exception in ExceptT on EOF" $ withTestStdin $ do
        Left result <- runExceptT $ flow $ clId @@ exceptClock
        isEOFError result @? "It's an EOF error"
    ]

-- ** 'CatchClock'

type TestCatchClock = CatchClock EClock IOError EClock

testClock :: TestCatchClock
testClock = CatchClock exceptClock $ const exceptClock

type ME = MaybeT E
type TestCatchClockMaybe = CatchClock EClock IOError (LiftClock E MaybeT (LiftClock IO (ExceptT IOError) Busy))

testClockMaybe :: TestCatchClockMaybe
testClockMaybe = CatchClock exceptClock (const (liftClock (liftClock Busy)))

catchClockTests :: TestTree
catchClockTests =
  testGroup
    "CatchClock"
    [ testCase "Outputs the exception of the second clock as well" $ withTestStdin $ do
        Left result <- runExceptT $ flow $ clId @@ testClock
        isEOFError result @? "It's an EOF error"
    , testCase "Can recover from an exception" $ withTestStdin $ do
        let stopInClsf :: ClSF ME TestCatchClockMaybe () ()
            stopInClsf = catchClSF clId $ constMCl empty
        result <- runExceptT $ runMaybeT $ flow_ $ stopInClsf @@ testClockMaybe
        result @?= Right Nothing
    ]

-- ** Clock failing at init

{- | This clock throws an exception at initialization.

Useful for testing clock initialization.
-}
data FailingClock = FailingClock

instance (Monad m) => Clock (ExceptT () m) FailingClock where
  type Time FailingClock = UTCTime
  type Tag FailingClock = ()
  initClock FailingClock = throwE ()

instance GetClockProxy FailingClock

type CatchFailingClock = CatchClock FailingClock () Busy

catchFailingClock :: CatchFailingClock
catchFailingClock = CatchClock FailingClock $ const Busy

failingClockTests :: TestTree
failingClockTests =
  testGroup
    "FailingClock"
    [ testCase "flow fails immediately" $ do
        result <- runExceptT $ flow_ $ clId @@ FailingClock
        result @?= Left ()
    , testCase "CatchClock recovers from failure at init" $ do
        let
          clsfStops :: ClSF (MaybeT IO) CatchFailingClock () ()
          clsfStops = catchClSF clId $ constM $ lift empty
        result <- runMaybeT $ flow_ $ clsfStops @@ catchFailingClock
        result @?= Nothing -- The ClSF stopped the execution, not the clock
    ]

-- ** 'DelayException'

type DelayedClock = DelayIOError StdinClock (Maybe [Text])

delayedClock :: DelayedClock
delayedClock = delayIOError StdinClock $ const Nothing

delayedClockTests :: TestTree
delayedClockTests =
  testGroup
    "DelayedClock"
    [ testCase "DelayException delays error by 1 step" $ withTestStdin $ do
        let
          throwCollectedText :: ClSF (ExceptT (Maybe [Text]) IO) DelayedClock () ()
          throwCollectedText = proc () -> do
            tag <- tagS -< ()
            textSoFar <- mappendS -< either (const []) pure tag
            throwOn' -< (isLeft tag, Just textSoFar)
        result <- runExceptT $ flow_ $ throwCollectedText @@ delayedClock
        result @?= Left (Just ["data", "test"])
    , testCase "DelayException throws error after 1 step" $ withTestStdin $ do
        let
          dontThrow :: ClSF (ExceptT (Maybe [Text]) IO) DelayedClock () ()
          dontThrow = clId
        result <- runExceptT $ flow_ $ dontThrow @@ delayedClock
        result @?= Left Nothing
    ]

-- ** Inner writer

{- | 'WriterT' is now the inner monad, meaning that the log survives exceptions.
This way, the state is not lost.
-}
type ClWriterExcept = DelayMonadIOError (ExceptT IOError (WriterT [Text] IO)) StdinClock IOError

clWriterExcept :: ClWriterExcept
clWriterExcept = delayMonadIOError' StdinClock

innerWriterTests :: TestTree
innerWriterTests = testCase "DelayException throws error after 1 step, but can write down results" $ withTestStdin $ do
  let
    tellStdin :: (MonadWriter [Text] m) => ClSF m ClWriterExcept () ()
    tellStdin = catchClSF (tagS >>> arrMCl (tell . pure)) clId

  (Left e, result) <- runWriterT $ runExceptT $ flow $ tellStdin @@ clWriterExcept
  isEOFError e @? "is EOF"
  result @?= ["test", "data"]

-- * Test helpers

-- | Emulate test standard input
withTestStdin :: IO a -> IO a
withTestStdin action = do
  testdataFile <- getDataFileName "test/assets/testdata.txt"
  withFile testdataFile ReadMode $ \h -> do
    hDuplicateTo h stdin
    action
