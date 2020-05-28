{- |
This module defines the 'TimeDomain' class.
Its instances model time.
Several instances such as 'UTCTime', 'Double' and 'Integer' are supplied here.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module FRP.Rhine.TimeDomain
  ( module FRP.Rhine.TimeDomain
  , UTCTime
  )
  where

-- base
import Data.Data

-- time
import Data.Time.Clock (UTCTime, diffUTCTime)

-- | A time domain is an affine space representing a notion of time,
--   such as real time, simulated time, steps, or a completely different notion.
class (Data time, Data (Diff time)) => TimeDomain time where
  type Diff time
  diffTime :: time -> time -> Diff time


instance TimeDomain UTCTime where
  type Diff UTCTime = Double
  diffTime t1 t2 = realToFrac $ diffUTCTime t1 t2

instance TimeDomain Double where
  type Diff Double = Double
  diffTime = (-)

instance TimeDomain Float where
  type Diff Float = Float
  diffTime = (-)

instance TimeDomain Integer where
  type Diff Integer = Integer
  diffTime          = (-)

instance TimeDomain () where
  type Diff () = ()
  diffTime _ _ = ()

-- | Any 'Num' can be wrapped to form a 'TimeDomain'.
newtype NumTimeDomain a = NumTimeDomain { fromNumTimeDomain :: a }
  deriving (Data, Num)

instance (Data a, Num a) => TimeDomain (NumTimeDomain a) where
  type Diff (NumTimeDomain a) = NumTimeDomain a
  diffTime = (-)
