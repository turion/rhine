module FRP.Rhine.SN.Type where

-- transformers
import Control.Monad.Trans.Reader (Reader)

-- automaton
import Data.Automaton

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy

-- FIXME Is it still applicable?

-- Andras Kovacs' trick: Encode in the domain

{- | An 'SN' is a side-effectful asynchronous /__s__ignal __n__etwork/,
where input, data processing (including side effects) and output
need not happen at the same time.

The type parameters are:

* 'm': The monad in which side effects take place.
* 'cl': The clock of the whole signal network.
        It may be sequentially or parallely composed from other clocks.
* 'a': The input type. Input arrives at the rate @In cl@.
* 'b': The output type. Output arrives at the rate @Out cl@.
-}
newtype SN m cl a b = SN {getSN :: Reader (Time cl) (Automaton m (Time cl, Tag cl, Maybe a) (Maybe b))}

instance (GetClockProxy cl) => ToClockProxy (SN m cl a b) where
  type Cl (SN m cl a b) = cl
