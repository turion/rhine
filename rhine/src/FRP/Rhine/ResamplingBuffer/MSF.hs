{-# LANGUAGE RecordWildCards #-}
module FRP.Rhine.ResamplingBuffer.MSF where

-- rhine
import FRP.Rhine

-- | Given a monadic stream function that accepts
--   a varying number of inputs (a list),
--   a `ResamplingBuffer` can be formed
--   that collects all input in a timestamped list.
msfBuffer
  :: Monad m
  => MSF m (TimeInfo cl2, [(TimeInfo cl1, a)]) b
  -- ^ The monadic stream function that consumes
  --   a single time stamp for the moment when an output value is required,
  --   and a list of timestamped inputs,
  --   and outputs a single value.
  --   The list will contain the /newest/ element in the head.
  -> ResamplingBuffer m cl1 cl2 a b
msfBuffer msf = msfBuffer' msf []
  where
    msfBuffer'
      :: Monad m
      => MSF m (TimeInfo cl2, [(TimeInfo cl1, a)]) b
      -> [(TimeInfo cl1, a)]
      -> ResamplingBuffer m cl1 cl2 a b
    msfBuffer' msf as = ResamplingBuffer {..}
      where
        put ti1 a = return $ msfBuffer' msf $ (ti1, a) : as
        get ti2   = do
          (b, msf') <- unMSF msf (ti2, as)
          return (b, msfBuffer msf')
