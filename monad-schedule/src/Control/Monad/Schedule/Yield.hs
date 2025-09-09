module Control.Monad.Schedule.Yield where

-- base
import qualified Control.Concurrent as Concurrent
import Control.Monad.IO.Class
import Data.Functor.Identity (Identity (runIdentity))

-- monad-schedule
import Control.Monad.Schedule.Trans

-- * 'YieldT'

-- | A monad for scheduling with cooperative concurrency.
type YieldT = ScheduleT ()

type Yield = YieldT Identity

-- | Let another thread wake up.
yield :: (Monad m) => YieldT m ()
yield = wait ()

runYieldT :: (Monad m) => YieldT m a -> m a
runYieldT = runScheduleT $ const $ return ()

runYield :: Yield a -> a
runYield = runIdentity . runYieldT

{- | Run a 'YieldT' value in a 'MonadIO',
  interpreting 'yield's as GHC concurrency yields.
-}
runYieldIO ::
  (MonadIO m) =>
  YieldT m a ->
  m a
runYieldIO = runScheduleT $ const $ liftIO Concurrent.yield
