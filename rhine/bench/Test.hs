import WordCount

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?), (@?=))

wordCount :: Int
wordCount = 966503

main =
  defaultMain $
    testGroup
      "WordCount"
      [ testCase "rhine" $ do
          countRhine <- rhineWordCount
          countRhine @?= wordCount
      , testCase "dunai" $ do
          countDunai <- dunaiWordCount
          countDunai @?= wordCount
      ]
