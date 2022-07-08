{- |
This module reexports most common names and combinators you will need to work with Rhine.
It does not export specific clocks, resampling buffers or schedules,
so you will have to import those yourself, e.g. like this:

@
{-# LANGUAGE DataKinds #-}
import FRP.Rhine
import FRP.Rhine.Clock.Realtime.Millisecond

main :: IO ()
main = flow $ constMCl (putStrLn "Hello World!") @@ (waitClock :: Millisecond 100)
@
-}
module FRP.Rhine (module X) where

-- dunai
import Data.MonadicStreamFunction         as X hiding ((>>>^), (^>>>))
import Data.VectorSpace                   as X

-- rhine
import FRP.Rhine.Clock                    as X
import FRP.Rhine.Clock.Proxy              as X
import FRP.Rhine.Clock.Util               as X
import FRP.Rhine.ClSF                     as X
import FRP.Rhine.Reactimation             as X
import FRP.Rhine.Reactimation.Combinators as X
import FRP.Rhine.ResamplingBuffer         as X
import FRP.Rhine.ResamplingBuffer.Util    as X
import FRP.Rhine.Schedule                 as X
import FRP.Rhine.SN                       as X
import FRP.Rhine.SN.Combinators           as X
import FRP.Rhine.Type                     as X

-- rhine (components)
import FRP.Rhine.Clock.FixedStep as X
import FRP.Rhine.Clock.Periodic as X
import FRP.Rhine.Clock.Realtime.Event as X
import FRP.Rhine.Clock.Realtime.Stdin as X
import FRP.Rhine.Clock.Realtime.Audio as X
import FRP.Rhine.Clock.Realtime.Busy as X
import FRP.Rhine.Clock.Realtime.Millisecond as X
import FRP.Rhine.Clock.Select as X

import FRP.Rhine.ResamplingBuffer.Interpolation as X
import FRP.Rhine.ResamplingBuffer.MSF as X
import FRP.Rhine.ResamplingBuffer.FIFO as X
import FRP.Rhine.ResamplingBuffer.LIFO as X
import FRP.Rhine.ResamplingBuffer.Collect as X
import FRP.Rhine.ResamplingBuffer.Timeless as X
import FRP.Rhine.ResamplingBuffer.KeepLast as X
