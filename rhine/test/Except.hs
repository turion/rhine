module Except where

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit

-- rhine
import FRP.Rhine
import Util (runScheduleRhinePure)

tests =
  testGroup
    "Except"
    [ testCase "Can raise and catch an exception" $ do
        let clsf = safely $ do
              try $ sinceInitS >>> throwOnCond (== 3) ()
              safe $ arr (const (-1))
        runScheduleRhinePure (clsf @@ FixedStep @1) (replicate 5 ()) @?= [Just 1, Just 2, Just (-1), Just (-1), Just (-1)]
    , testCase "Can raise and catch very many exceptions without steps in between" $ do
        let clsf = safely $ go 100000
            go n = do
              _ <- try $ throwOnCond (< n) ()
              go $ n - 1
            inputs = [0]
        runScheduleRhinePure (clsf @@ FixedStep @1) inputs @?= [Just 0]
    ]
