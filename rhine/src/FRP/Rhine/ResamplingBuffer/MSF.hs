{- |
Collect and process all incoming values statefully and with time stamps.
-}

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
msfBuffer = msfBuffer' []
  where
    msfBuffer'
      :: Monad m
      => [(TimeInfo cl1, a)]
      -> MSF m (TimeInfo cl2, [(TimeInfo cl1, a)]) b
      -> ResamplingBuffer m cl1 cl2 a b
    msfBuffer' as msf = ResamplingBuffer {..}
      where
        put ti1 a = return $ msfBuffer' ((ti1, a) : as) msf
        get ti2   = do
          (b, msf') <- unMSF msf (ti2, as)
          return (b, msfBuffer msf')
