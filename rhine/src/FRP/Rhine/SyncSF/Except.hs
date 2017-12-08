{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}

module FRP.Rhine.SyncSF.Except
  ( module FRP.Rhine.SyncSF.Except
  , module X
  , safe, safely, Empty, exceptS, runMSFExcept
  )
  where

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except as X
import Control.Monad.Trans.Reader

-- dunai
import Control.Monad.Trans.MSF.Except hiding (try, once, once_, throwOn, throwOn', throwS)
-- TODO Find out whether there is a cleverer way to handle exports
import qualified Control.Monad.Trans.MSF.Except as MSFE

-- rhine
import FRP.Rhine
import FRP.Rhine.SyncSF.Except.Util

-- * Types

{- | A synchronous exception-throwing signal function.
It is based on a @newtype@, 'MSFExcept',
to exhibit a monad interface /in the exception type/.
`return` then corresponds to throwing an exception,
and `(>>=)` is exception handling.
(For more information, see the documentation of 'MSFExcept'.)

* @m@:  The monad that the signal function may take side effects in
* @cl@: The clock on which the signal function ticks
* @a@:  The input type
* @b@:  The output type
* @e@:  The type of exceptions that can be thrown
-}
type SyncExcept m cl a b e = MSFExcept (ReaderT (TimeInfo cl) m) a b e

{- | A clock polymorphic 'SyncExcept'.
Any clock with time domain @td@ may occur.
-}
type BehaviourFExcept m td a b e
  = forall cl. td ~ TimeDomainOf cl => SyncExcept m cl a b e

-- | Compatibility to U.S. american spelling.
type BehaviorFExcept m td a b e = BehaviourFExcept m td a b e



commuteExceptReader :: ExceptT e (ReaderT r m) a -> ReaderT r (ExceptT e m) a
commuteExceptReader a = ReaderT $ \r -> ExceptT $ runReaderT (runExceptT a) r

runSyncExcept :: Monad m => SyncExcept m cl a b e -> SyncSF (ExceptT e m) cl a b
runSyncExcept = liftMSFPurer commuteExceptReader . runMSFExcept

-- | Enter the monad context in the exception
--   for |SyncSF|s in the |ExceptT| monad.
--   The 'SyncSF' will be run until it encounters an exception.
try :: Monad m => SyncSF (ExceptT e m) cl a b -> SyncExcept m cl a b e
try = MSFE.try . liftMSFPurer commuteReaderExcept

-- | Within the same tick, perform a monadic action,
--   and immediately throw the value as an exception.
once :: Monad m => (a -> m e) -> SyncExcept m cl a b e
once f = MSFE.once $ lift . f

-- | A variant of |once| without input.
once_ :: Monad m => m e -> SyncExcept m cl a b e
once_ = once . const

-- | Immediately throw the exception on the input.
throwS :: Monad m => SyncSF (ExceptT e m) cl e a
throwS = arrMSync throwE

-- | Throw the given exception when the 'Bool' turns true.
throwOn :: Monad m => e -> SyncSF (ExceptT e m) cl Bool ()
throwOn e = proc b -> throwOn' -< (b, e)

-- | Variant of 'throwOn', where the exception can vary every tick.
throwOn' :: Monad m => SyncSF (ExceptT e m) cl (Bool, e) ()
throwOn' = proc (b, e) -> if b
  then throwS  -< e
  else returnA -< ()


-- | Advances a single tick with the given Kleisli arrow,
--   and then throws an exception.
step :: Monad m => (a -> m (b, e)) -> SyncExcept m cl a b e
step f = MSFE.step $ lift . f

-- | Remembers and indefinitely outputs the first input value.
keepFirst :: Monad m => SyncSF m cl a a
keepFirst = safely $ do
  a <- try throwS
  safe $ arr $ const a


-- | Throws an exception after the specified time difference,
--   outputting the remaining time difference.
timer
  :: ( Monad m
     , TimeDomain td
     , Ord (Diff td)
     )
  => Diff td
  -> BehaviorF (ExceptT () m) td a (Diff td)
timer diff = proc _ -> do
  time      <- timeInfoOf absolute -< ()
  startTime <- keepFirst           -< time
  let remainingTime = time `diffTime` startTime
  _         <- throwOn ()          -< remainingTime > diff
  returnA                          -< remainingTime

-- | Like 'timer', but divides the remaining time by the total time.
scaledTimer
  :: ( Monad m
     , TimeDomain td
     , Fractional (Diff td)
     , Ord        (Diff td)
     )
  => Diff td
  -> BehaviorF (ExceptT () m) td a (Diff td)
scaledTimer diff = timer diff >>> arr (/ diff)
