module Stream where

-- base
import Control.Monad.Identity (Identity (..))

-- transformers
import Control.Monad.Trans.Writer.Lazy (runWriter, tell)

-- selective
import Control.Selective

-- tasty
import Test.Tasty (testGroup)

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- automaton
import Automaton
import Data.Stream (constM, snapshot, streamToList, unfold)
import Data.Stream.Result

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
    , testGroup
        "snapshot"
        [ testCase "Shows the current effect in the output" $
            let stream = snapshot $ constM $ tell [()]
             in take 3 (fmap runWriter $ fst $ runWriter $ streamToList stream) @?= [((), [()]), ((), [()]), ((), [()])]
        ]
    ]
