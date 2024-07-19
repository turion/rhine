{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- | Utilities to run 'ClSF's at the speed of combined clocks
   when they are defined only for a constituent clock.
-}
module FRP.Rhine.ClSF.Upsample where

-- profunctors
import Data.Profunctor.Traversing (traverse')

-- automaton
import Data.Automaton.Trans.Reader

-- rhine
import FRP.Rhine.ClSF.Core
import FRP.Rhine.SN.Tick

-- Note that the Semigroup instance of Either a arbitrary
-- updates when the first argument is Right.

{- | Upsample a 'ClSF' to a parallel clock.
   The given 'ClSF' is only called when @clR@ ticks,
   otherwise the last output is replicated
   (with the given @b@ as initialisation).
-}
upsample ::
  (Monad m, HasClocks clsSub cls) =>
  ClsSF m clsSub a b ->
  ClsSF m cls a [b]
upsample clsf = readerS $ arr (\(tick, a) -> (, a) <$> projectTick tick) >>> traverse' (runReaderS clsf)
