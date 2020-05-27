{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Collect and process all incoming values statefully and with time stamps.
-}
module FRP.Rhine.ResamplingBuffer.MSF where

-- base
import Data.Data

-- dunai
import Data.MonadicStreamFunction.InternalCore

-- rhine
import FRP.Rhine.ResamplingBuffer

{- | Given a monadic stream function that accepts
  a varying number of inputs (a list),
  a `ResamplingBuffer` can be formed
  that collects all input in a timestamped list.
-}
msfBuffer ::
  ( Monad m
  , Data cl1
  , Data (Diff (Time cl1))
  , Data (Time cl1)
  , Data (Tag cl1)
  , Data a
  ) =>
-- | The monadic stream function that consumes
  --   a single time stamp for the moment when an output value is required,
  --   and a list of timestamped inputs,
  --   and outputs a single value.
  --   The list will contain the /newest/ element in the head.
  MSF m (TimeInfo cl2, [(TimeInfo cl1, a)]) b ->
  ResamplingBuffer m cl1 cl2 a b
msfBuffer Cell {..} = ResamplingBuffer {resamplingState = ([], cellState), ..}
  where
    put (as, s) ti1 a = return ((ti1, a) : as, s)
    get (as, s) ti2 = do
      (b, s') <- cellStep s (ti2, as)
      return (b, ([], s))
msfBuffer arrm@ArrM {} = msfBuffer $ toCell arrm
