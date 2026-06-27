-- rhine

import Sum
import WordCount

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- | The number of words in Project Gutenberg's edition of Shakespeare's complete works.
wordCount :: Int
wordCount = 966503

main :: IO ()
main =
  defaultMain $
    testGroup
      "Benchmark tests"
      [ testGroup
          "WordCount"
          [ testCase "rhine" $ rhineWordCount >>= (@?= wordCount)
          ]
      , testGroup
          "Sum"
          [ testCase "rhine flow" $ Sum.rhineFlow Sum.nMax @?= Sum.direct Sum.nMax
          , testCase "rhine IO" $ Sum.rhineIO >>= (@?= Sum.direct Sum.nMax)
          , testCase "automaton reactimate" $ Sum.automatonReactimate Sum.nMax @?= Sum.direct Sum.nMax
          , testCase "automaton reactimate IO" $ Sum.automatonReactimateIO >>= (@?= Sum.direct Sum.nMax)
          ]
      ]
