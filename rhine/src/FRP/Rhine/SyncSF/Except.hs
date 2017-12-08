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


type SyncExcept m cl a b e = MSFExcept (ReaderT (TimeInfo cl) m) a b e

type BehaviourFExcept m td a b e
  = forall cl. td ~ TimeDomainOf cl => SyncExcept m cl a b e

-- | Compatibility to U.S. american spelling.
type BehaviorFExcept m td a b e = BehaviourFExcept m td a b e


-- | Commute the effects of the |ReaderT| and the |ExceptT| monad.
commuteReaderExcept :: ReaderT r (ExceptT e m) a -> ExceptT e (ReaderT r m) a
commuteReaderExcept a = ExceptT $ ReaderT $ \r -> runExceptT $ runReaderT a r

-- | Enter the monad context in the exception
--   for |SyncSF|s in the |ExceptT| monad.
try :: Monad m => SyncSF (ExceptT e m) cl a b -> SyncExcept m cl a b e
try = MSFE.try . liftMSFPurer commuteReaderExcept

-- | Within the same tick, perform a monadic action,
--   and immediately throw the value as an exception.
once :: Monad m => (a -> m e) -> SyncExcept m cl a b e
once f = MSFE.once $ lift . f

-- | A variant of |once| without input.
once_ :: Monad m => m e -> SyncExcept m cl a b e
once_ = once . const

-- |
throwS :: Monad m => SyncSF (ExceptT e m) cl e a
throwS = arrMSync throwE

throwOn' :: Monad m => SyncSF (ExceptT e m) cl (Bool, e) ()
throwOn' = proc (b, e) -> if b
  then throwS  -< e
  else returnA -< ()

throwOn :: Monad m => e -> SyncSF (ExceptT e m) cl Bool ()
throwOn e = proc b -> throwOn' -< (b, e)


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
--   outputting the time passed since the 'timer' was instantiated.
timer
  :: ( Monad m
     , TimeDomain td
     , Ord (Diff td)
     )
  => Diff td
  -> BehaviorF (ExceptT () m) td a (Diff td)
timer diff = proc _ -> do
  sinceSimStart <- timeSinceSimStart -< ()
  _             <- throwOn ()        -< sinceSimStart > diff
  returnA                            -< sinceSimStart

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
