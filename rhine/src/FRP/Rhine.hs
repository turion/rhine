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
import Data.MonadicStreamFunction as X

-- rhine
import FRP.Rhine.Clock            as X
import FRP.Rhine.Reactimation     as X
import FRP.Rhine.ResamplingBuffer as X
import FRP.Rhine.Schedule         as X
import FRP.Rhine.SF               as X
import FRP.Rhine.SF.Combinators   as X
import FRP.Rhine.ClSF           as X
