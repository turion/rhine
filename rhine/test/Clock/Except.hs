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
import Test.Tasty (testGroup)

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?), (@?=))

-- rhine
import FRP.Rhine
import FRP.Rhine.Clock.Except (CatchClock (CatchClock), DelayIOError, DelayMonadIOError, ExceptClock (ExceptClock), delayIOError, delayMonadIOError')
import Paths_rhine

-- FIXME organisation: group functions & clock values closer to their test cases
type E = ExceptT IOError IO
type WT = WriterT [Text]
type M = WT E
type EClock = ExceptClock StdinClock IOError

type TestClock =
  LiftClock
    E
    WT
    ( CatchClock
        EClock
        IOError
        EClock
    )

-- FIXME also need to test the other branch of CatchClock
testClock :: TestClock
testClock = liftClock $ CatchClock (ExceptClock StdinClock) $ const $ ExceptClock StdinClock

clsf :: ClSF M TestClock () ()
clsf = proc () -> do
  tag <- tagS -< ()
  arrMCl tell -< either (const ["weird"]) pure tag

type DelayedClock = DelayIOError StdinClock (Maybe [Text])

-- type DelayedClock = DelayException IO UTCTime (ExceptClock StdinClock IOError) IOError (Maybe [Text])

delayedClock :: DelayedClock
delayedClock = delayIOError StdinClock $ const Nothing

-- FIXME it would be cool if there were a utility that combines two clsfs under the two parts of the catchclock
clsf2 :: ClSF (ExceptT (Maybe [Text]) IO) DelayedClock () ()
clsf2 = proc () -> do
  tag <- tagS -< ()
  textSoFar <- mappendS -< either pure (const []) tag
  throwOn' -< (isRight tag, Just textSoFar)

clsf3 :: ClSF (ExceptT (Maybe [Text]) IO) DelayedClock () ()
clsf3 = proc () -> do
  tag <- tagS -< ()
  _textSoFar <- mappendS -< either pure (const []) tag
  returnA -< ()

-- clsf4 :: ClSF (ExceptT IOError (WriterT [Text] IO)) (LiftClock (WriterT [Text] IO) (ExceptT IOError) (DelayIOError StdinClock IOError)) () ()
clsf4 :: (Tag cl ~ Either Text a) => (MonadWriter [Text] m) => ClSF m cl () ()
clsf4 =
  tagS >>> proc tag -> case tag of
    Left text -> arrMCl tell -< [text]
    Right _ -> returnA -< ()

tests =
  testGroup
    "ExceptClock"
    [ testCase "Outputs the exception on EOF" $ withTestStdin $ do
        Left result <- runExceptT $ runWriterT $ flow $ clsf @@ testClock
        isEOFError result @? "It's an EOF error"
    , testCase "DelayException delays error by 1 step" $ withTestStdin $ do
        result <- runExceptT $ flow $ clsf2 @@ delayedClock
        result @?= Left (Just ["data", "test"])
    , testCase "DelayException throws error after 1 step" $ withTestStdin $ do
        result <- runExceptT $ flow $ clsf3 @@ delayedClock
        result @?= Left Nothing
    , testCase "DelayException throws error after 1 step, but can write down results" $ withTestStdin $ do
        (Left e, result) <- runWriterT $ runExceptT $ flow $ clsf4 @@ clWriterExcept
        isEOFError e @? "is EOF"
        result @?= ["test", "data"]
    ]

clWriterExcept :: DelayMonadIOError (ExceptT IOError (WriterT [Text] IO)) StdinClock IOError
clWriterExcept = delayMonadIOError' StdinClock

withTestStdin :: IO a -> IO a
withTestStdin action = do
  testdataFile <- getDataFileName "test/assets/testdata.txt"
  withFile testdataFile ReadMode $ \h -> do
    hDuplicateTo h stdin
    action
