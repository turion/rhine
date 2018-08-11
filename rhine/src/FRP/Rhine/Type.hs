{- |
The type of a complete Rhine program:
A signal network together with a matching clock value.
-}

module FRP.Rhine.Type where

-- rhine
import FRP.Rhine.ClockTree
import FRP.Rhine.SN

{- |
An 'SN' together with a clock tree of matching topology 'cto',
A 'Rhine' is a reactive program, possibly with open inputs and outputs.
If the input and output types 'a' and 'b' are both '()',
that is, the 'Rhine' is "closed",
then it is a standalone reactive program
that can be run with the function 'flow'.
-}
data Rhine m (cto :: ClockTopology) a b = Rhine
  { sn    :: SN m cto a b
  , clock :: ClockTree m cto
  }
