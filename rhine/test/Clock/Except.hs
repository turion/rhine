{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Clock.Except where

-- base
import Data.Either (isRight)
import GHC.IO.Handle (hDuplicateTo)
import System.IO (IOMode (ReadMode), stdin, withFile)
import System.IO.Error (isEOFError)

-- mtl
import Control.Monad.Writer.Class

-- transformers
-- Replace Strict by CPS when bumping mtl to 2.3
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
    "ExceptClock"
    [catchClockTests, delayedClockTests, innerWriterTests]

-- ** 'CatchClock'

type WT = WriterT [Text]
type E = ExceptT IOError IO
type M = WT E
type EClock = ExceptClock StdinClock IOError

type TestClock =
  LiftClock
    E
    WT
    ( CatchClock
        EClock
        IOError
        StdinClock
    )

-- FIXME also need to test the other branch of CatchClock
testClock :: TestClock
testClock = liftClock $ CatchClock (ExceptClock StdinClock) $ const StdinClock

catchClockTests :: TestTree
catchClockTests =
  testGroup
    "CatchClock"
    [ testCase "Outputs the exception on EOF" $ withTestStdin $ do
        let
          tellStdin :: ClSF M TestClock () ()
          tellStdin = proc () -> do
            tag <- tagS -< ()
            arrMCl tell -< either (const []) pure tag

        Left result <- runExceptT $ runWriterT $ flow $ tellStdin @@ testClock
        isEOFError result @? "It's an EOF error"
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
          -- FIXME it would be cool if there were a utility that combines two clsfs under the two parts of the catchclock
          throwCollectedText :: ClSF (ExceptT (Maybe [Text]) IO) DelayedClock () ()
          throwCollectedText = proc () -> do
            tag <- tagS -< ()
            textSoFar <- mappendS -< either (const []) pure tag
            throwOn' -< (isRight tag, Just textSoFar)
        result <- runExceptT $ flow $ throwCollectedText @@ delayedClock
        result @?= Left (Just ["data", "test"])
    , testCase "DelayException throws error after 1 step" $ withTestStdin $ do
        let
          dontThrow :: ClSF (ExceptT (Maybe [Text]) IO) DelayedClock () ()
          dontThrow = clId
        result <- runExceptT $ flow $ dontThrow @@ delayedClock
        result @?= Left Nothing
    ]

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
