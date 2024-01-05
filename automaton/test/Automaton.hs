{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Automaton where

-- base
import Control.Applicative (Alternative (..))
import Control.Arrow
import Data.Functor.Identity (runIdentity)
import Data.List (uncons)
import Data.Maybe (maybeToList)

-- transformers
import Control.Monad.State.Strict

-- selective
import Control.Selective ((<*?))

-- tasty
import Test.Tasty (testGroup)

-- tasty-quickcheck
import Test.Tasty.QuickCheck

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- automaton
import Automaton.Except
import Data.Automaton
import Data.Automaton.Final
import Data.Automaton.Trans.Maybe

tests =
  testGroup
    "Automaton"
    [ testGroup
        "Alternative"
        [ testGroup
            "<|>"
            [ testProperty "has same semantics as final" $
                \(input :: [(Maybe Int, Maybe Int)]) ->
                  embed ((arr fst >>> inMaybe) <|> (arr snd >>> inMaybe)) input
                    === embed (fromFinal $ (arr fst >>> toFinal inMaybe) <|> (arr snd >>> toFinal inMaybe)) input
            ]
        , testGroup
            "some"
            [ testCase "Maybe" $ embed (some $ arrM id) [Nothing] @?= (Nothing :: Maybe [[()]])
            , testCase "Parser" $ runParser (embed (some $ constM aChar) [(), ()]) "hi" @?= [(["h", "i"], "")]
            ]
        , testGroup
            "many"
            [ testCase "Maybe" $ embed (many $ arrM id) [Nothing] @?= (Just [[]] :: Maybe [[()]])
            , testCase "Parser" $ runParser (many (char 'h')) "hi" @?= [("h", "i"), ("", "hi")]
            ]
        ]
    , testGroup
        "parallely"
        [ testCase "Outputs separate sums" $ runIdentity (embed (parallely sumN) [[], [], [1, 2], [10, 20], [100], [], [1000, 200]]) @?= [[], [], [1, 2], [11, 22], [111], [], [1111, 222]]
        ]
    , testGroup
        "Selective"
        [ testCase "selects second Automaton conditionally" $
            runIdentity (embed (right sumN <*? arr (const (* 2))) [Right 1, Right 2, Left 10, Right 3, Left 20]) @?= [1, 3, 20, 6, 40]
        ]
    , testCase "count" $ runIdentity (embed count [(), (), ()]) @?= [1, 2, 3]
    , testCase "delay" $ runIdentity (embed (count >>> delay 0) [(), (), ()]) @?= [0, 1, 2]
    , testCase "sumS" $ runIdentity (embed (arr (const (1 :: Float)) >>> sumS) [(), (), ()]) @?= [1, 2, 3]
    , testCase "sumN" $ runIdentity (embed (arr (const (1 :: Integer)) >>> sumN) [(), (), ()]) @?= [1, 2, 3]
    , Automaton.Except.tests
    ]

inMaybe :: Automaton Maybe (Maybe a) a
inMaybe = hoistS (runIdentity . runMaybeT) inMaybeT

-- * Parser helper type to test many & some

newtype Parser a = Parser {getParser :: StateT String [] a}
  deriving (Functor, Applicative, Monad, Alternative)

runParser :: Parser a -> String -> [(a, String)]
runParser = runStateT . getParser

aChar :: Parser Char
aChar = Parser $ StateT $ maybeToList . uncons

char :: Char -> Parser Char
char c = do
  c' <- aChar
  guard $ c == c'
  return c
