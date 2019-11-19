{- |
The type of a complete Rhine program:
A signal network together with a matching clock value.
-}

module FRP.Rhine.Type where

import FRP.Rhine.SN

{- |
A 'Rhine' consists of a 'SN' together with a clock of matching type 'cl'.
It is a reactive program, possibly with open inputs and outputs.
If the input and output types 'a' and 'b' are both '()',
that is, the 'Rhine' is "closed",
then it is a standalone reactive program
that can be run with the function 'flow'.
-}
data Rhine m cl a b = Rhine
  { sn    :: SN m cl a b
  , clock :: cl
  }
