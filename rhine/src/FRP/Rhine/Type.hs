{- |
The type of a complete Rhine program:
A signal network together with a matching clock value.
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module FRP.Rhine.Type where

-- transformers
import Control.Monad.Trans.Class

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Schedule
import FRP.Rhine.SN

{- |
A 'Rhine' consists of un 'SN' together with a clock of matching type 'cl'.
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

-- * Hoist 'Rhine's along monad morphisms

hoistSeqRhine
  :: ( Monad m, Monad m'
     , cl1 ~ In cl1, cl1 ~ Out cl1
     , cl2 ~ In cl2, cl2 ~ Out cl2
     )
  => (forall x . m x -> m' x)
  -> Rhine m (SequentialClock m cl1 cl2) a b
  -> Rhine m' (SequentialClock m' (HoistClock m m' cl1) (HoistClock m m' cl2)) a b
hoistSeqRhine monadMorphism Rhine {..} = Rhine
  { sn    = hoistSeqSN monadMorphism sn
  , clock = hoistedSeqClock monadMorphism clock
  }

liftSeqRhine
  :: ( Monad m, MonadTrans t, Monad (t m)
     , cl1 ~ In cl1, cl1 ~ Out cl1
     , cl2 ~ In cl2, cl2 ~ Out cl2
     )
  => Rhine m (SequentialClock m cl1 cl2) a b
  -> Rhine (t m) (SequentialClock (t m) (LiftClock m t cl1) (LiftClock m t cl2)) a b
liftSeqRhine = hoistSeqRhine lift
