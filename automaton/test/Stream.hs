module Stream where

-- base
import Data.Functor.Identity (Identity (..))

-- selective
import Control.Selective

-- tasty
import Test.Tasty (testGroup)

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- automaton
import Automaton
import Data.Stream (streamToList, unfold, unfold_, mmap, handleExceptT, handleCompose, snapshotCompose, hoist')
import Data.Stream.Result
import Control.Monad.Trans.Except (throwE)
import Control.Monad (when)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Functor.Compose (Compose(..))

tests =
  testGroup
    "Stream"
    [ Automaton.tests
    , testGroup
        "Selective"
        [ testCase "Selects second stream based on first stream" $
            let automaton1 = unfold 0 (\n -> Result (n + 1) (if even n then Right n else Left n))
                automaton2 = pure (* 10)
             in take 10 (runIdentity (streamToList (automaton1 <*? automaton2))) @?= [0, 10, 2, 30, 4, 50, 6, 70, 8, 90]
        , testCase "Progresses state of second stream only when first stream returns Left" $
            let automaton1 = unfold 0 (\n -> Result (n + 1) (if even n then Right n else Left n))
                automaton2 = unfold 1 (\n -> Result (n + 2) (* n))
             in take 10 (runIdentity (streamToList (automaton1 <*? automaton2))) @?= [0, 1, 2, 9, 4, 25, 6, 49, 8, 81]
        ]
    , testCase
        "handleExceptT" $ let exceptionAfter2 = mmap (\n -> when (n == 2) $ throwE ()) $ unfold_ 0 (+1)
            in take 5 (runIdentity (streamToList (handleExceptT exceptionAfter2))) @?= [Right (),Left (),Left (),Left (),Left ()]
    , testCase
        "snapshotCompose" $ let asManyAsN = hoist' (Compose . Identity) $ mmap (\n -> NonEmpty.fromList [0..n]) $ unfold_ 0 (+1)
            in take 5 (runIdentity (streamToList (hoist' (fmap NonEmpty.head  . getCompose) (snapshotCompose asManyAsN)))) @?= [0 :| [1],0 :| [1,2],0 :| [1,2,3],0 :| [1,2,3,4],0 :| [1,2,3,4,5]]

    ]
