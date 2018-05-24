{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module FRP.Rhine.Clock.FixedRate where


-- rhine
import FRP.Rhine


-- | A side-effect-free clock ticking at a fixed rate.
newtype FixedRate = FixedRate Double

instance Monad m => Clock m FixedRate where
  type Time FixedRate = Double
  type Tag  FixedRate = ()
  startClock (FixedRate timeStep) = return
    ( arr (const timeStep) >>> sumS &&& arr (const ())
    , 0
    )
