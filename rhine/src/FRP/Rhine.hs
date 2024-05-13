{- |
This module reexports most common names and combinators you will need to work with Rhine.
It also exports most specific clocks and resampling buffers,
so you can import everything in one line:

@
import FRP.Rhine

main :: IO ()
main = flow \$ constMCl (putStrLn \"Hello World!\") \@\@ (waitClock :: Millisecond 100)
@
-}
module FRP.Rhine (module X) where

-- automaton
import Data.Automaton as X

-- rhine
import Data.VectorSpace as X
import FRP.Rhine.ClSF as X
import FRP.Rhine.Clock as X
import FRP.Rhine.Clock.Proxy as X
import FRP.Rhine.Clock.Util as X
import FRP.Rhine.Reactimation as X
import FRP.Rhine.Reactimation.Combinators as X
import FRP.Rhine.ResamplingBuffer as X
import FRP.Rhine.ResamplingBuffer.Util as X
import FRP.Rhine.SN as X
import FRP.Rhine.SN.Combinators as X
import FRP.Rhine.Schedule as X
import FRP.Rhine.Type as X

-- rhine (components)
import FRP.Rhine.Clock.FixedStep as X
import FRP.Rhine.Clock.Periodic as X
import FRP.Rhine.Clock.Realtime.Audio as X
import FRP.Rhine.Clock.Realtime.Busy as X
import FRP.Rhine.Clock.Realtime.Event as X
import FRP.Rhine.Clock.Realtime.Millisecond as X
import FRP.Rhine.Clock.Realtime.Never as X
import FRP.Rhine.Clock.Realtime.Stdin as X
import FRP.Rhine.Clock.Select as X
import FRP.Rhine.Clock.Trivial as X
import FRP.Rhine.Clock.Unschedule as X

import FRP.Rhine.ResamplingBuffer.ClSF as X
import FRP.Rhine.ResamplingBuffer.Collect as X
import FRP.Rhine.ResamplingBuffer.FIFO as X
import FRP.Rhine.ResamplingBuffer.Interpolation as X
import FRP.Rhine.ResamplingBuffer.KeepLast as X
import FRP.Rhine.ResamplingBuffer.LIFO as X
import FRP.Rhine.ResamplingBuffer.Timeless as X
