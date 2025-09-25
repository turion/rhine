{-# LANGUAGE StandaloneDeriving #-}

module Test.Control.Monad.Trans.List where -- FIXME should I maybe move all test files in that hierarchy?

-- base
import Control.Applicative (Alternative ((<|>)))
import Data.Bifunctor (first)
import Data.Functor.Identity (Identity (runIdentity))

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.Lazy (Writer, WriterT (..), tell) -- Using lazy writer here so I can deal with lazy infinite streams

-- tasty
import Test.Tasty (testGroup)

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- automaton
import Control.Monad.Trans.List
import Data.Functor.Classes (Eq1, Show1)
import Data.Stream (unfold_)
import Data.Stream.Except (StreamExcept (CoalgebraicExcept), stepInstant)
import Data.Stream.Optimized (OptimizedStreamT (..), mmap)
import Data.Stream.Result (Result (..))

type M = ListT (Writer [String])

run :: M a -> ([a], [String])
run = runIdentity . runWriterT . runListT

{- HLINT ignore "Use newtype instead of data" -}
data RecursiveListT m a = RecursiveListT (m (Maybe (a, RecursiveListT m a)))

deriving instance (Eq1 m, Eq a) => Eq (RecursiveListT m a)
deriving instance (Show1 m, Show a) => Show (RecursiveListT m a)

recursive :: (Monad m) => ListT m a -> RecursiveListT m a
recursive = recursive' . getListT
  where
    recursive' mas = RecursiveListT $ do
      result <- stepInstant mas
      pure $ case result of
        Left () -> Nothing
        Right (Result mas' a) -> Just (a, recursive' mas')

sample :: M Int
sample = listT [pure 0, tell ["Hi"] >> pure 1, tell ["World"] >> pure 2] <|> pure 3 <|> (lift (tell ["!"]) >> pure 4)

sampleNumbers = [0, 1, 2, 3, 4]
sampleLog = ["Hi", "World", "!"]

{- HLINT ignore tests "Redundant <$>" -}
{- HLINT ignore tests "Redundant pure" -}
{- HLINT ignore tests "Use $>" -}
{- HLINT ignore tests "Functor law" -}
tests =
  testGroup
    "Control.Monad.Trans.List"
    [ testGroup
        "runListT"
        [ testCase "Empty" $ run (listT []) @?= ([] :: [Int], [])
        , testCase "Single element" $ run (listT [pure 3]) @?= ([3], [])
        , testCase "sample" $ run sample @?= ([0, 1, 2, 3, 4], ["Hi", "World", "!"])
        ]
    , testCase "listT" $ run (listT [pure 0, tell ["Hi"] >> pure 1, tell ["World"] >> pure 2]) @?= ([0, 1, 2], ["Hi", "World"])
    , testGroup
        "Functor"
        [ testCase "fmap" $ run ((2 *) <$> sample) @?= first (fmap (2 *)) (run sample)
        , testCase "fmap law" $ recursive ((2 *) <$> (2 *) <$> sample) @?= recursive ((4 *) <$> sample)
        ]
    , testGroup
        "Applicative"
        [ testCase "pure" $ run (pure 3 :: M Int) @?= ([3], [])
        , testGroup
            "<*>"
            [ testCase "pure pure" $ recursive (pure 3 *> pure 4) @?= recursive (pure 4 :: M Int)
            , testCase "pure fmap" $ recursive ((+) <$> pure 1 <*> pure 2) @?= recursive (pure 3 :: M Int)
            , testCase "pure sample" $ recursive ((+) <$> sample <*> pure 1) @?= recursive ((+) <$> pure 1 <*> sample)
            , testCase "sample sample" $ run ((+) <$> sample <*> sample) @?= ((+) <$> sampleNumbers <*> sampleNumbers, [])
            ]
        ]
    , testGroup
        "Infinite lists"
        [ testCase "listT" $ let (ns, logs) = run $ listT $ (\n -> tell [show n] >> pure n) <$> [0 ..] in (take 5 ns, take 5 logs) @?= ([1, 2, 3, 4, 5], ["0", "1", "2", "3", "4", "5"])
        [ testCase "infinite stream" $ let (ns, logs) = run $ ListT $ CoalgebraicExcept $ mmap (\n -> lift (tell [show n]) >> pure n) $ Stateful $ unfold_ 0 (+ 1) in take 5 ns @?= ([1, 2, 3, 4, 5], ["1", "2", "3", "4", "5"])
        ]
    ]
