{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
module Control.Monad.Event where

-- base
import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..), fromList)

-- containers
import Data.Sequence ((<|), Seq (..))

-- transformers
import Control.Monad.Trans.Class

-- operational
import Control.Monad.Operational

-- rhine
import Control.Monad.Schedule.Class

-- * 'Event'-based scheduling

-- | Event broadcasting and listening operations
data Event ev a where
  -- | Emit the event @ev@ to all listeners.
  Emit :: ev -> Event ev ()
  -- | Listen if an event occurs.
  Listen :: Event ev (Maybe ev)

{- |
Values in 'EventT' are computations that can emit events and block on a listening operation.
When scheduling, the events serve as a "fence",
effectively synchronising all threads.
-}
type EventT ev m = ProgramT (Event ev) m

-- | Emit an event to all threads.
emit :: Monad m => ev -> EventT ev m ()
emit = singleton . Emit

-- | Block until an event is emitted.
listen :: Monad m => EventT ev m (Maybe ev)
listen = singleton Listen

-- | Block until a specific event is emitted.
listenUntil :: Monad m => (ev -> Maybe a) -> EventT ev m a
listenUntil f = do
  evMaybe <- listen
  case evMaybe >>= f of
    Nothing -> listenUntil f
    Just a -> return a

-- | Run 'EventT' single threaded.
--   Aborts if there are more listens than events.
runEventTWith :: Monad m => Seq ev -> EventT ev m a -> m (a, [ev])
runEventTWith events = viewT >=> eval events
  where
    eval :: Monad m => Seq ev -> ProgramViewT (Event ev) m a -> m (a, [ev])
    eval events (Return a) = return (a, toList events)
    eval events (Emit ev :>>= f) = runEventTWith (ev <| events) $ f ()
    eval (ev :<| events) (Listen :>>= f) = runEventTWith events $ f $ Just ev
    eval Empty (Listen :>>= f) = runEventTWith Empty $ f Nothing

instance (Monad m, MonadSchedule m) => MonadSchedule (EventT ev m) where
  schedule = broadcastUntilReturn >>> lift
    where
      partitionEventT
        :: [ProgramViewT (Event ev) m a]
        -> ([a], [(ev, EventT ev m a)], [Maybe ev -> EventT ev m a])
      partitionEventT = foldr prepend ([], [], [])

      prepend
        :: ProgramViewT (Event ev) m a
        -> ([a], [(ev, EventT ev m a)], [Maybe ev -> EventT ev m a])
        -> ([a], [(ev, EventT ev m a)], [Maybe ev -> EventT ev m a])
      prepend (Return a) (as, emits, listens) = (a : as, emits, listens)
      prepend (Emit ev :>>= f) (as, emits, listens) = (as, (ev, f ()) : emits, listens)
      prepend (Listen :>>= f) (as, emits, listens) = (as, emits, f : listens)

      broadcastOnce
        :: ev
        -> [Maybe ev -> EventT ev m a]
        -> [EventT ev m a]
      broadcastOnce ev listens = ($ Just ev) <$> listens

      mkEmit :: Monad m => (ev, EventT ev m a) -> EventT ev m a
      mkEmit (ev, action) = emit ev >> action

      mkListen :: Monad m => (Maybe ev -> EventT ev m a) -> EventT ev m a
      mkListen f = listen >>= f

      broadcastUntilReturn
        :: (Monad m, MonadSchedule m)
        => NonEmpty (EventT ev m a)
        -> m (NonEmpty a, [EventT ev m a])
      broadcastUntilReturn actions = do
        (views, delayed) <- schedule $ viewT <$> actions
        let delayed' = liftMView <$> delayed
        case partitionEventT $ toList views of
          -- Some actions have finally returned. The other actions will be wrapped again and processed later.
          ((a : as), emits, listens) -> return (a :| as, delayed' ++ (mkEmit <$> emits) ++ (mkListen <$> listens))
          -- No actions have returned yet, but one has emitted an event.
          -- Broadcast it to all listeners.
          ([], (ev, action) : emits, listens)
            -> broadcastUntilReturn $ fromList
              $  action
              :  delayed'
              ++ (mkEmit <$> emits)
              ++ broadcastOnce ev listens
          -- FIXME Better to do this with sequences
          -- No actions have returned yet, and all listeners are waiting.
          -- Unblock them by telling them that there will be no event now.
          ([], [], listens) -> broadcastUntilReturn $ fromList
            $ delayed'
            ++ (($ Nothing) <$> listens)

-- * 'ProgramT' utilities

liftView :: Monad m => ProgramViewT instr m a -> ProgramT instr m a
liftView (Return a) = return a
liftView (instr :>>= f) = singleton instr >>= f

liftMView :: Monad m => m (ProgramViewT instr m a) -> ProgramT instr m a
liftMView action = join $ lift $ liftView <$> action
