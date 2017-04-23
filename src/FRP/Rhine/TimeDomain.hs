{-# LANGUAGE TypeFamilies #-}
module FRP.Rhine.TimeDomain where

class TimeDomain td where
  type Diff td
  diffTime :: td -> td -> Diff td
