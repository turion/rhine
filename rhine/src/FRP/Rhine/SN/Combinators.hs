{-# LANGUAGE GADTs #-}
module FRP.Rhine.SN.Combinators where


-- rhine
import qualified FRP.Rhine.ClSF as ClSF
import FRP.Rhine.ResamplingBuffer.Util
import FRP.Rhine.SN



-- | Compose two signal networks on the same clock in parallel.
--   At one tick of @cl@, both networks are stepped.
(****)
  :: Monad m
  => SN m cl  a      b
  -> SN m cl     c      d
  -> SN m cl (a, c) (b, d)
Synchronous clsf1 **** Synchronous clsf2 = Synchronous $ clsf1 ClSF.*** clsf2
Sequential sn11 rb1 sn12 **** Sequential sn21 rb2 sn22 = Sequential sn1 rb sn2
  where
    sn1 = sn11 **** sn21
    sn2 = sn12 **** sn22
    rb  = rb1 *-* rb2
Parallel sn11 sn12 **** Parallel sn21 sn22
  = Parallel (sn11 **** sn21) (sn12 **** sn22)
-- Note that the patterns above are the only ones that can occur.
-- This is ensured by the clock constraints in the SF constructors.
_ **** _ = error "Impossible pattern in ****"
