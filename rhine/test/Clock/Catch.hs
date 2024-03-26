{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Clock.Catch where

-- base
import Control.Exception
import Data.Bifunctor (first)
import GHC.IO.Handle (hDuplicateTo)
import System.IO (IOMode (ReadMode), stdin, withFile)
import System.IO.Error (isEOFError)

-- text
import Data.Text

-- tasty
import Test.Tasty (testGroup)

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- rhine
import FRP.Rhine
import FRP.Rhine.Clock.Catch
import Paths_rhine

type CatchStdin = CatchOnceIOError StdinClock

newtype MyException = MyException [Text]
  deriving (Show)

instance Exception MyException

cl :: CatchStdin
cl = catchOnce StdinClock isEOFError

clsf :: ClSF IO CatchStdin () ()
clsf = proc () -> do
  tag <- tagS -< ()
  allText <- mappendS -< either (const []) pure tag
  left $ arrMCl $ Control.Exception.throw . MyException -< Data.Bifunctor.first (const allText) tag
  returnA -< ()

tests =
  testGroup
    "Catch"
    [ testCase "Outputs the exception on EOF" $ do
        testdataFile <- getDataFileName "test/assets/testdata.txt"
        withFile testdataFile ReadMode $ \h -> do
          hDuplicateTo h stdin
          catch (flow $ clsf @@ cl) $ \(MyException outputs) ->
            outputs @?= ["data", "test"]
    ]
