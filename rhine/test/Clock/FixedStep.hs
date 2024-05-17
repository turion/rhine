{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Clock.FixedStep where

-- vector-sized
import Data.Vector.Sized (toList)

-- tasty
import Test.Tasty (testGroup)

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- rhine
import FRP.Rhine
import Util

tests =
  testGroup
    "Clock.FixedStep"
    [ testCase "Outputs linearly increasing ticks" $
        let
          output = runScheduleRhinePure (absoluteS @@ (FixedStep @5)) $ replicate 4 ()
         in
          output @?= Just <$> [5, 10, 15, 20]
    , testCase "Outputs linearly increasing ticks" $
        let
          output = runScheduleRhinePure (sinceInitS @@ (FixedStep @5)) $ replicate 4 ()
         in
          output @?= Just <$> [5, 10, 15, 20]
    , testCase "Outputs scheduled ticks in order" $
        let
          output = runScheduleRhinePure ((absoluteS @@ (FixedStep @5)) |@| (absoluteS @@ (FixedStep @3))) $ replicate 6 ()
         in
          output @?= Just <$> [3, 5, 6, 9, 10, 12]
    , testCase "Outputs scheduled ticks in order (mirrored)" $
        let
          output = runScheduleRhinePure ((absoluteS @@ (FixedStep @3)) |@| (absoluteS @@ (FixedStep @5))) $ replicate 6 ()
         in
          output @?= Just <$> [3, 5, 6, 9, 10, 12]
    , testCase "Resamples correctly" $
        let
          output = fmap (fmap (first toList)) $ runScheduleRhinePure ((absoluteS @@ (FixedStep @3)) >-- downsampleFixedStep --> ((clId &&& absoluteS) @@ (FixedStep @12))) $ replicate 10 ()
         in
          output
            @?= [ Nothing
                , Nothing
                , Nothing
                , Nothing
                , Just ([12, 9, 6, 3], 12)
                , Nothing
                , Nothing
                , Nothing
                , Nothing
                , Just ([24, 21, 18, 15], 24)
                ]
    ]
