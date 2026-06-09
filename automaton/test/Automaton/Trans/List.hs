module Automaton.Trans.List where

-- base
import Control.Monad.Identity (runIdentity)

-- tasty
import Test.Tasty
import Test.Tasty.HUnit

-- automaton
import Control.Arrow (arr)
import Data.Automaton (arrM, count, embed, mapS)
import Data.Automaton.Trans.List (fromFoldable, sequenceS, widthFirst)

tests :: TestTree
tests =
  testGroup
    "Trans.List"
    [ testGroup
        "widthFirst"
        [ testCase "branching automaton" $
            runIdentity (embed (widthFirst (arrM (\n -> fromFoldable [1 .. (n :: Int)]))) [2, 1])
              @?= [[1, 2], [1, 1]]
        , testCase "no outputs" $
            runIdentity (embed (widthFirst (arrM $ const $ fromFoldable ([] :: [Int]))) [1, 2, 3])
              @?= [[], [], []]
        ]
    , testGroup
        "sequenceS"
        [ testCase "single automaton" $
            runIdentity (embed (widthFirst (sequenceS [arr (+ 1)])) [1, 2, 3])
              @?= [[2], [3], [4]]
        , testCase "multiple counters" $
            runIdentity (embed (widthFirst (sequenceS [count, count, count])) [(), (), ()])
              @?= [[1, 1, 1], [2, 2, 2], [3, 3, 3]]
        ]
    , testGroup
        "mapS"
        [ testCase "stateless" $
            runIdentity (embed (mapS (arr (+ 1))) [[1, 2], [3, 4]])
              @?= [[2, 3], [4, 5]]
        , testCase "stateful" $
            runIdentity (embed (mapS count) [[(), (), ()], [(), ()]])
              @?= [[1, 2, 3], [4, 5]]
        ]
    ]
