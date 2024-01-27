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
      "WordCount"
      [ testCase "rhine" $ rhineWordCount >>= (@?= wordCount)
      , testCase "dunai" $ dunaiWordCount >>= (@?= wordCount)
      ]
