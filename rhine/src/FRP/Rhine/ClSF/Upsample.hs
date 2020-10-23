-- | Utilities to run 'ClSF's at the speed of combined clocks
--   when they are defined only for a constituent clock.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module FRP.Rhine.ClSF.Upsample where

-- base
import Data.Semigroup

-- dunai
import Control.Monad.Trans.MSF.Reader
--import Data.MonadicStreamFunction

-- rhine
import FRP.Rhine.ClSF.Core
import FRP.Rhine.Schedule

-- | An 'MSF' can be given arbitrary other arguments
--   that cause it to tick without doing anything
--   and replicating the last output.
upsampleMSF :: Monad m => b -> MSF m a b -> MSF m (Either arbitrary a) b
upsampleMSF b msf = right msf >>> accumulateWith (<>) (Right b) >>> arr fromRight
  where
    fromRight (Right b') = b'
    fromRight (Left  _ ) = error "fromRight: This case never occurs in upsampleMSF."
-- Note that the Semigroup instance of Either a arbitrary
-- updates when the first argument is Right.


-- | Upsample a 'ClSF' to a parallel clock.
--   The given 'ClSF' is only called when @clR@ ticks,
--   otherwise the last output is replicated
--   (with the given @b@ as initialisation).
upsampleR
  :: (Monad m, Time clL ~ Time clR)
  => b -> ClSF m clR a b -> ClSF m (ParallelClock clL clR) a b
upsampleR b clsf = readerS $ arr remap >>> upsampleMSF b (runReaderS clsf)
  where
    remap (TimeInfo { tag = Left  tag     }, _) = Left tag
    remap (TimeInfo { tag = Right tag, .. }, a) = Right (TimeInfo { .. }, a)


-- | Upsample a 'ClSF' to a parallel clock.
--   The given 'ClSF' is only called when @clL@ ticks,
--   otherwise the last output is replicated
--   (with the given @b@ as initialisation).
upsampleL
  :: (Monad m, Time clL ~ Time clR)
  => b -> ClSF m clL a b -> ClSF m (ParallelClock clL clR) a b
upsampleL b clsf = readerS $ arr remap >>> upsampleMSF b (runReaderS clsf)
  where
    remap (TimeInfo { tag = Right tag     }, _) = Left tag
    remap (TimeInfo { tag = Left  tag, .. }, a) = Right (TimeInfo { .. }, a)
