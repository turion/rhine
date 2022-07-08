{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

import FRP.Rhine

-- | Create a simple message containing the time stamp since initialisation,
--   for each tick of the clock.
--   Since 'createMessage' works for arbitrary clocks (and doesn't need further input data),
--   it is a 'Behaviour'.
--   @time@ is the 'TimeDomain' of any clock used to sample,
--   and it needs to be constrained in order for time differences
--   to have a 'Show' instance.
createMessage
  :: (Monad m, Show (Diff time))
  => String
  -> Behaviour m time String
createMessage str
  =   timeInfoOf sinceInit >-> arr show
  >-> arr (("Clock " ++ str ++ " has ticked at: ") ++)

-- | Specialise 'createMessage' to a specific clock,
--   ticking twice a second.
ms500 :: ClSF IO (Millisecond 500) () String
ms500 = createMessage "500 MS"

-- | Specialise 'createMessage' to a different clock,
--   ticking every 1.2 seconds.
ms1200 :: ClSF IO (Millisecond 1200) () String
ms1200 = createMessage "1200 MS"


-- | Output a message /every second/ (= every 1000 milliseconds).
--   Let us assume we want to assure that 'printEverySecond'
--   is only called every second,
--   then we constrain its type signature with the clock @Millisecond 1000@.
printEverySecond :: Show a => ClSF IO (Millisecond 1000) a ()
printEverySecond = arrMCl print

-- | Create messages every 500 ms and every 1200 ms,
--   collecting all of them in a list,
--   which is output at every second.
main :: IO ()
main = flow $
  ms500 @@ waitClock |@| ms1200 @@ waitClock
  >-- collect  -->
  printEverySecond @@ waitClock


-- | Rhine prevents the consumption of a signal at a different clock than it is created,
--   if no explicit resampling strategy is given.
--   /One does not simply create and consume data at different rates./
--   Uncomment the following for a type error (the clocks don't match).

-- typeError = ms500 >>> printEverySecond
