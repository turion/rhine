{- | Wrapper library to write @gloss@ Gloss applications in Rhine.

A pure Rhine app with @gloss@ backend must use the 'GlossClock' or 'GlossCombinedClock'
(from 'FRP.Rhine.Gloss.Pure.Single' and 'FRP.Rhine.Gloss.Pure.Combined', respectively),
since the @gloss@ API only offers callbacks.
In order to run such a reactive program, you have to use 'flowGloss' or 'flowGlossCombined'.

A more flexible alternative, at the cost of introducing 'IO' concurrency,
is the 'FRP.Rhine.Gloss.IO' wrapper.
There, you can combine the @gloss@ clocks with arbitrary other 'IO' clocks.
-}
module FRP.Rhine.Gloss (module X) where

import Control.Arrow as X

-- rhine
import FRP.Rhine as X

-- rhine-gloss

import FRP.Rhine.Gloss.Common as X
import FRP.Rhine.Gloss.IO as X
import FRP.Rhine.Gloss.Pure as X
import FRP.Rhine.Gloss.Pure.Combined as X
