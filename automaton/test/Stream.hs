module Stream where

-- base
import Control.Monad (when)
import Control.Monad.Identity (Identity (..))

-- transformers
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Writer.Lazy (runWriter, tell)

-- selective
import Control.Selective

-- tasty
import Test.Tasty (testGroup)

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- automaton
import Data.Stream (StreamT, constM, handleExceptT, handleWriterT, mmap, snapshot, streamToList, unfold, unfold_)
import Data.Stream.Result

tests =
  testGroup
    "Stream"
    [ testGroup
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
    , testGroup
        "handleEffect"
        [ testGroup
            "handleExceptT"
            [ testCase "Switches to constantly Left after exception has been triggered" $
                let stream = mmap (\i -> when (i > 2) (throwE ())) nats
                 in take 5 (runIdentity $ streamToList $ handleExceptT stream) @?= [Right (), Right (), Left (), Left (), Left ()]
            ]
        , testGroup
            "handleWriterT"
            [ testCase "Returns the current log on the output" $
                let stream = mmap (tell . pure) nats
                 in take 3 (fmap fst $ runIdentity $ streamToList $ handleWriterT stream) @?= [[1], [1, 2], [1, 2, 3]]
            ]
        ]
    ]

nats :: (Applicative m) => StreamT m Int
nats = unfold_ 0 (+ 1)
