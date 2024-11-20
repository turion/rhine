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
          [ testCase "rhine" $ Sum.rhine Sum.nMax @?= Sum.direct Sum.nMax
          , testCase "automaton" $ Sum.automaton Sum.nMax @?= Sum.direct Sum.nMax
          , testCase "automatonNoEmbed" $ Sum.automatonNoEmbed Sum.nMax @?= Sum.direct Sum.nMax
          , testCase "automatonEmbed" $ Sum.automatonEmbed Sum.nMax @?= Sum.direct Sum.nMax
          , testCase "automatonNoEmbedInlined" $ Sum.automatonNoEmbedInlined Sum.nMax @?= Sum.direct Sum.nMax
          , testCase "rhine flow" $ Sum.rhineFlow Sum.nMax @?= Sum.direct Sum.nMax
          ]
      ]
