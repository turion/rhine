{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

-- FIXME haddocks
module Data.Automaton.Schedule where

-- base
import Control.Arrow
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, readMVar, takeMVar, tryTakeMVar)
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity (..))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose (..))
import Data.Kind (Type)
import Data.List.NonEmpty as N
import Data.Maybe (fromMaybe, maybeToList)

-- base-compat
import Data.Foldable1 (Foldable1 (foldrMap1))

-- transformers
import Control.Monad.Trans.Accum (AccumT (..), runAccumT)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.Writer.CPS qualified as CPS
import Control.Monad.Trans.Writer.Lazy qualified as Lazy
import Control.Monad.Trans.Writer.Strict qualified as Strict

-- sop-core
import Data.SOP (HCollapse (hcollapse), HSequence (htraverse'), I (..), K (..), NP (..), SListI, hmap, hzipWith)

-- free
import Control.Monad.Trans.Free (FreeF (..), FreeT (..), iterT, liftF)

-- automaton

import Control.Monad.Trans.Changeset (ChangesetT (..))
import Data.Automaton (Automaton (..), arrM, constM, feedback, handleAutomaton, initialised_, liftS, reactimate, withAutomaton_)
import Data.Automaton qualified as Automaton
import Data.Automaton.Trans.Except (exceptS)
import Data.Automaton.Trans.Maybe (runMaybeS)
import Data.Automaton.Trans.Reader (readerS, runReaderS)
import Data.Monoid.RightAction (RightAction)
import Data.Stream (StreamT (..), concatS)
import Data.Stream.Optimized (OptimizedStreamT (Stateful), toStreamT)
import Data.Stream.Result
import Data.Tuple (swap)

class MonadSchedule m where
  -- | Run a nonempty list of automata concurrently.
  schedule :: NonEmpty (Automaton m a b) -> Automaton m a b

-- | Start all streams in the background and send their values to a shared 'MVar'.
instance MonadSchedule IO where
  schedule automata = proc a -> do
    (output, input) <- initialised_ startStreams -< ()
    arrM $ void . tryTakeMVar -< input
    arrM $ uncurry putMVar -< (input, a)
    arrM takeMVar -< output
    where
      startStreams = do
        output <- newEmptyMVar
        input <- newEmptyMVar
        forM_ automata $ \automaton -> forkIO $ reactimate $ lastMVarValue input >>> automaton >>> arrM (putMVar output)
        return (output, input)
      lastMVarValue var = feedback Nothing $ proc ((), aMaybe) -> do
        case aMaybe of
          Nothing -> do
            a <- constM $ readMVar var -< ()
            returnA -< (a, Just a)
          Just a -> do
            aNewMaybe <- constM $ tryTakeMVar var -< ()
            let aNew = fromMaybe a aNewMaybe
            returnA -< (aNew, aNewMaybe)

instance (Monad m, MonadSchedule m) => MonadSchedule (ReaderT r m) where
  schedule =
    fmap runReaderS
      >>> schedule
      >>> readerS

instance (Monad m, MonadSchedule m) => MonadSchedule (ExceptT e m) where
  schedule =
    fmap exceptS
      >>> schedule
      >>> withAutomaton_ (fmap sequenceA >>> ExceptT)

instance (Monad m, MonadSchedule m) => MonadSchedule (MaybeT m) where
  schedule =
    fmap runMaybeS
      >>> schedule
      >>> withAutomaton_ (fmap sequenceA >>> MaybeT)

instance (Monoid w, Monad m, MonadSchedule m) => MonadSchedule (CPS.WriterT w m) where
  schedule =
    fmap (withAutomaton_ (CPS.runWriterT >>> fmap (\(Result s a, w) -> Result s (a, w))))
      >>> schedule
      >>> withAutomaton_ (fmap (\(Result s (a, w)) -> (Result s a, w)) >>> CPS.writerT)

instance (Monoid w, Monad m, MonadSchedule m) => MonadSchedule (Strict.WriterT w m) where
  schedule =
    fmap (withAutomaton_ (Strict.runWriterT >>> fmap (\(Result s a, w) -> Result s (a, w))))
      >>> schedule
      >>> withAutomaton_ (fmap (\(Result s (a, w)) -> (Result s a, w)) >>> Strict.WriterT)

instance (Monoid w, Monad m, MonadSchedule m) => MonadSchedule (Lazy.WriterT w m) where
  schedule =
    fmap (withAutomaton_ (Lazy.runWriterT >>> fmap (\(Result s a, w) -> Result s (a, w))))
      >>> schedule
      >>> withAutomaton_ (fmap (\(Result s (a, w)) -> (Result s a, w)) >>> Lazy.WriterT)

-- | This will share the accumulated log from the past with all automata
instance (Monoid w, Monad m, MonadSchedule m) => MonadSchedule (AccumT w m) where
  schedule =
    fmap (withAutomaton_ (runAccumT >>> ReaderT >>> CPS.writerT))
      >>> schedule
      >>> withAutomaton_ (CPS.runWriterT >>> runReaderT >>> AccumT)

-- | This will share the accumulated state from the past with all automata
instance (Monoid w, RightAction w s, Monad m, MonadSchedule m) => MonadSchedule (ChangesetT s w m) where
  schedule =
    fmap (withAutomaton_ (getChangesetT >>> ReaderT >>> fmap swap >>> CPS.writerT))
      >>> schedule
      >>> withAutomaton_ (CPS.runWriterT >>> fmap swap >>> runReaderT >>> ChangesetT)

-- | Cycle through all automata in a round-robin fashion
instance MonadSchedule Identity where
  schedule =
    fmap (getAutomaton >>> toStreamT)
      >>> foldrMap1 buildStreams consStreams
      >>> roundRobinStreams
      >>> fmap N.toList
      >>> concatS
      >>> Stateful
      >>> Automaton
    where
      buildStreams :: StreamT m b -> Streams m b
      buildStreams StreamT {state, step} =
        Streams
          { states = I state :* Nil
          , steps = Step (ResultStateT step) :* Nil
          }

      consStreams :: StreamT m b -> Streams m b -> Streams m b
      consStreams StreamT {state, step} Streams {states, steps} =
        Streams
          { states = I state :* states
          , steps = Step (ResultStateT step) :* steps
          }

-- FIXME take care to reverse & test

roundRobinStreams :: (Functor m, Applicative m) => Streams m b -> StreamT m (NonEmpty b)
roundRobinStreams Streams {states, steps} =
  StreamT
    { state = states
    , step = \s ->
        s
          & hzipWith (\Step {getStep} (I s) -> getResultStateT getStep s <&> RunningResult & Compose) steps
          & htraverse' getCompose
          <&> ( \results ->
                  Result
                    (results & hmap (getRunningResult >>> resultState >>> I))
                    (results & hmap (getRunningResult >>> output >>> K) & hnonemptycollapse)
              )
    }

hnonemptycollapse :: (SListI as) => NP (K b) (a ': as) -> NonEmpty b
hnonemptycollapse (K a :* as) = a :| hcollapse as

-- | A nonempty list of 'StreamT's, unzipped into their states and their steps.
data Streams m b
  = forall state (states :: [Type]).
  (SListI states) =>
  Streams
  { states :: NP I (state ': states)
  , steps :: NP (Step m b) (state ': states)
  }

-- | One step of a stream, with the state type argument going last, so it is usable with sop-core.
newtype Step m b state = Step {getStep :: ResultStateT state m b}

-- | The result of a stream, with the type arguments swapped, so it's usable with sop-core
newtype RunningResult b state = RunningResult {getRunningResult :: Result state b}

-- * The symbolic effect of skipping one step of an automaton

newtype SkipT m a = SkipT {getSkipT :: FreeT Identity m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)

type Yield = SkipT Identity

runSkipS :: (Functor m, Monad m) => Automaton (SkipT m) a b -> Automaton m a (Maybe b)
runSkipS = handleAutomaton $ \StreamT {state, step} ->
  StreamT
    { state = step state
    , step = \s -> ReaderT $ \a -> do
        oneTick <- runFreeT $ getSkipT $ runReaderT s a
        return $ case oneTick of
          Pure (Result s' b) -> Result (step s') (Just b)
          Free (Identity cont) -> Result (lift $ SkipT cont) Nothing
    }

instance (Monad m, MonadSchedule m) => MonadSchedule (SkipT m) where
  schedule = fmap runSkipS >>> schedule >>> fmap maybeToList >>> Automaton.concatS >>> liftS

skip :: (Monad m) => SkipT m ()
skip = SkipT $ liftF $ pure ()

runSkipT :: (Monad m) => SkipT m a -> m a
runSkipT = iterT runIdentity . getSkipT

runSkipTWith :: (Monad m) => m () -> SkipT m a -> m a
runSkipTWith action = iterT (\ima -> action >> runIdentity ima) . getSkipT

runYield :: Yield a -> a
runYield = runIdentity . runSkipT
