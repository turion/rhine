{-# LANGUAGE Arrows #-}
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
-- once f = MSFE.once $ lift . f -- TODO dunai 0.1.2
once f = try $ arrMSync (lift . f) >>> throwS

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


-- ** Will be in dunai-0.1.2

-- | Advances a single tick with the given Kleisli arrow,
--   and then throws an exception.
step :: Monad m => (a -> m (b, e)) -> SyncExcept m cl a b e
step f = try $ proc a -> do
  n      <- count               -< ()
  (b, e) <- arrMSync (lift . f) -< a
  _      <- throwOn'            -< (n > (1 :: Int), e)
  returnA                       -< b

-- | Immediately throw the current input as an exception.
currentInput :: Monad m => SyncExcept m cl e b e
currentInput = try throwS
