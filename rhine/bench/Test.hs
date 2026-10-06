-- rhine
import Sum
import SumMultirate
import WordCount

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- | The number of words in Project Gutenberg's edition of Shakespeare's complete works.
wordCount :: Int
wordCount = 966503

expected = Sum.direct Sum.nMax
expectedMultirate = SumMultirate.direct SumMultirate.nMax

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
          [ testCase "rhine flow" $ Sum.rhineFlow Sum.nMax @?= expected
          , testCase "rhine IO" $ Sum.rhineIO >>= (@?= expected)
          , testCase "automaton reactimate" $ Sum.automatonReactimate Sum.nMax @?= expected
          , testCase "automaton reactimate IO" $ Sum.automatonReactimateIO >>= (@?= expected)
          ]
      , testGroup
          "Sum multirate"
          [ testCase "rhine Busy Waitclock" $ SumMultirate.rhineBusyWaitclock SumMultirate.nMax >>= (@?= expectedMultirate)
          , testCase "rhine Busy Busy" $ SumMultirate.rhineBusyBusy SumMultirate.nMax >>= (@?= expectedMultirate)
          , testCase "rhine FixedStep 1:1 IO" $ SumMultirate.rhineFixedStep11IO SumMultirate.nMax >>= (@?= expectedMultirate)
          , testCase "automaton reactimate" $ SumMultirate.automatonReactimate SumMultirate.nMax >>= (@?= expectedMultirate)
          , testCase "rhine FixedStep 1000:1" $ SumMultirate.rhineFixedStep10001 SumMultirate.nMax @?= expectedMultirate
          , testCase "rhine FixedStep 1:1" $ SumMultirate.rhineFixedStep11 SumMultirate.nMax @?= expectedMultirate
          , testCase "rhine FixedStep 1000:1 downsampleFixedStep" $ SumMultirate.rhineFixedStep11000downsampleFixedStep SumMultirate.nMax @?= expectedMultirate
          , testCase "rhine Trivial Trivial" $ SumMultirate.rhineTrivialTrivial SumMultirate.nMax @?= expectedMultirate
          , testCase "rhine single threaded" $ SumMultirate.rhineSingleThreaded SumMultirate.nMax @?= expectedMultirate
          ]
      ]
