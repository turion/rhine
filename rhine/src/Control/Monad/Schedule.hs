{-# LANGUAGE DeriveFunctor #-}

{- |
This module supplies a general purpose monad transformer
that adds a syntactical "delay", or "waiting" side effect.

This allows for universal and deterministic scheduling of clocks
that implement their waiting actions in 'ScheduleT'.
See 'FRP.Rhine.Schedule.Trans' for more details.
-}
module Control.Monad.Schedule where

-- base
import Control.Concurrent

-- transformers
import Control.Monad.IO.Class

-- free
import Control.Monad.Trans.Free

-- TODO Implement Time via StateT

{- |
A functor implementing a syntactical "waiting" action.

* 'diff' represents the duration to wait.
* 'a' is the encapsulated value.
-}
data Wait diff a = Wait diff a
  deriving (Functor)

{- |
Values in @ScheduleT diff m@ are delayed computations with side effects in 'm'.
Delays can occur between any two side effects, with lengths specified by a 'diff' value.
These delays don't have any semantics, it can be given to them with 'runScheduleT'.
-}
type ScheduleT diff = FreeT (Wait diff)

-- | The side effect that waits for a specified amount.
wait :: Monad m => diff -> ScheduleT diff m ()
wait diff = FreeT $ return $ Free $ Wait diff $ return ()

{- | Supply a semantic meaning to 'Wait'.
   For every occurrence of @Wait diff@ in the @ScheduleT diff m a@ value,
   a waiting action is executed, depending on 'diff'.
-}
runScheduleT :: Monad m => (diff -> m ()) -> ScheduleT diff m a -> m a
runScheduleT waitAction = iterT $ \(Wait n ma) -> waitAction n >> ma

{- | Run a 'ScheduleT' value in a 'MonadIO',
   interpreting the times as milliseconds.
-}
runScheduleIO ::
  (MonadIO m, Integral n) =>
  ScheduleT n m a ->
  m a
runScheduleIO = runScheduleT $ liftIO . threadDelay . (* 1000) . fromIntegral

-- TODO The definition and type signature are both a mouthful. Is there a simpler concept?

{- | Runs two values in 'ScheduleT' concurrently
   and returns the first one that yields a value
   (defaulting to the first argument),
   and a continuation for the other value.
-}
race ::
  (Ord diff, Num diff, Monad m) =>
  ScheduleT diff m a ->
  ScheduleT diff m b ->
  ScheduleT
    diff
    m
    ( Either
        (a, ScheduleT diff m b)
        (ScheduleT diff m a, b)
    )
race (FreeT ma) (FreeT mb) = FreeT $ do
  -- Perform the side effects to find out how long each 'ScheduleT' values need to wait.
  aWait <- ma
  bWait <- mb
  case aWait of
    -- 'a' doesn't need to wait. Return immediately and leave the continuation for 'b'.
    Pure a -> return $ Pure $ Left (a, FreeT $ return bWait)
    -- 'a' needs to wait, so we need to inspect 'b' as well and see which one needs to wait longer.
    Free (Wait aDiff aCont) -> case bWait of
      -- 'b' doesn't need to wait. Return immediately and leave the continuation for 'a'.
      Pure b -> return $ Pure $ Right (wait aDiff >> aCont, b)
      -- Both need to wait. Which one needs to wait longer?
      Free (Wait bDiff bCont) ->
        if aDiff <= bDiff
          then -- 'a' yields first, or both are done simultaneously.
          runFreeT $ do
            -- Perform the wait action that we've deconstructed
            wait aDiff
            -- Recurse, since more wait actions might be hidden in 'a' and 'b'. 'b' doesn't need to wait as long, since we've already waited for 'aDiff'.
            race aCont $ wait (bDiff - aDiff) >> bCont
          else -- 'b' yields first. Analogously.
          runFreeT $ do
            wait bDiff
            race (wait (aDiff - bDiff) >> aCont) bCont

-- | Runs both schedules concurrently and returns their results at the end.
async ::
  (Ord diff, Num diff, Monad m) =>
  ScheduleT diff m a ->
  ScheduleT diff m b ->
  ScheduleT diff m (a, b)
async aSched bSched = do
  ab <- race aSched bSched
  case ab of
    Left (a, bCont) -> do
      b <- bCont
      return (a, b)
    Right (aCont, b) -> do
      a <- aCont
      return (a, b)
