{-# LANGUAGE Arrows                #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module FRP.Rhine.Clock.Step where


-- base
import GHC.TypeLits


-- rhine
import FRP.Rhine


-- | A pure (side effect free) clock ticking at multiples of 'n'.
--   The tick rate is in the type signature,
--   which prevents composition of signals at different rates.
data Step (n :: Nat) where
  Step :: KnownNat n => Step n -- TODO Does the constraint bring any benefit?

-- | Extract the type-level natural number as an integer.
stepsize :: Step n -> Integer
stepsize step@Step = natVal step

instance Monad m => Clock m (Step n) where
  type TimeDomainOf (Step n) = Integer
  type Tag          (Step n) = ()
  startClock cl = return
    ( count >>> arr (* stepsize cl)
      &&& arr (const ())
    , 0
    )


-- | Two 'Step' clocks can always be scheduled without side effects.
scheduleStep
  :: Monad m
  => Schedule m (Step n1) (Step n2)
scheduleStep = Schedule f where
  f cl1 cl2 = return (msf, 0)
    where
      n1 = stepsize cl1
      n2 = stepsize cl2
      msf = concatS $ proc _ -> do
        k <- arr (+1) <<< count -< ()
        returnA                 -< [ (k, Left  ()) | k `mod` n1 == 0 ]
                                ++ [ (k, Right ()) | k `mod` n2 == 0 ]


-- * To be ported to dunai

-- TODO Will be in dunai
concatS :: Monad m => MSF m () [b] -> MSF m () b
concatS msf = MSF $ \_ -> tick msf []
  where
    tick msf (b:bs) = return (b, MSF $ \_ -> tick msf bs)
    tick msf []     = do
      (bs, msf') <- unMSF msf ()
      tick msf' bs
