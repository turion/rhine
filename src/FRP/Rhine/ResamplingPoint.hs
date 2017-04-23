module FRP.Rhine.ResamplingPoint where

import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.Schedule



data ResamplingPoint m cl1 cl2 a b = ResamplingPoint
  { buffer   :: ResamplingBuffer m cl1 cl2 a b
  , schedule :: Schedule m cl1 cl2
  }

(-@-) :: ResamplingBuffer m cl1 cl2 a b -> Schedule m cl1 cl2 -> ResamplingPoint m cl1 cl2 a b
(-@-) = ResamplingPoint
