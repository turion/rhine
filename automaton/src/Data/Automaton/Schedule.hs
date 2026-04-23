{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

-- FIXME haddocks
module Data.Automaton.Schedule where

-- base
import Control.Arrow
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, guard, replicateM_)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity (..))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose (..))
import Data.Kind (Type)
import Data.List.NonEmpty as N
import Data.Maybe (maybeToList)

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
import Data.Automaton (Automaton (..), arrM, constM, feedback, handleAutomaton, liftS, reactimate, withAutomaton_, initialised)
import Data.Automaton qualified as Automaton
import Data.Automaton.Trans.Except (exceptS)
import Data.Automaton.Trans.Maybe (runMaybeS)
import Data.Automaton.Trans.Reader (readerS, runReaderS)
import Data.Monoid.RightAction (RightAction)
import Data.Stream (StreamT (..), concatS)
import Data.Stream.Optimized (OptimizedStreamT (Stateful), toStreamT)
import Data.Stream.Result
import Data.Tuple (swap)
import Control.Monad.Schedule.Trans (ScheduleT, Wait (..), wait)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Sequence (Seq, viewl, ViewL (..))
import qualified Data.Bifunctor as Bifunctor
import Witherable ((<&?>))
import Data.TimeDomain (TimeDifference(..))
import qualified Data.Sequence as Seq
import Data.Automaton.Trans.State (runStateS__, modify)
import Control.Monad.Trans.State.Strict (StateT (..), get)
import qualified Data.List as List

class MonadSchedule m where
  -- | Run a nonempty list of automata concurrently.
  schedule :: NonEmpty (Automaton m a b) -> Automaton m a b

-- | Start all streams in the background and send their values to a shared 'MVar'.
instance MonadSchedule IO where
  schedule automata = proc a -> do
    (output, input) <- initialised startStreams -< a
    arrM $ uncurry putMVar -< (input, a)
    arrM takeMVar -< output
    where
      startStreams a0 = do
        output <- newEmptyMVar
        input <- newEmptyMVar
        forkIO $ replicateM_ (N.length automata - 1) $ putMVar input a0
        forM_ automata $ \automaton -> forkIO $ reactimate $ constM (takeMVar input) >>> automaton >>> arrM (putMVar output)
        return (output, input)

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

-- FIXME this stops all threads when one is finished. Maybe should let all the other finish as well?
-- Same question for ExceptT
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

-- | Break down the steps of an 'Automaton' in 'ScheduleT' into waiting effects and returning values
runScheduleS :: (Functor m, Monad m) => Automaton (ScheduleT diff m) a b -> Automaton m a (Either diff b)
runScheduleS = handleAutomaton $ \StreamT {state, step} ->
  StreamT
    { state = step state
    , step = \s -> ReaderT $ \a -> do
        oneStep <- runFreeT $ runReaderT s a
        return $ case oneStep of
          Pure (Result s' b) -> Result (step s') (Right b)
          Free (Wait diff cont) -> Result (lift cont) (Left diff)
    }

scheduleS :: Monad m => Automaton m a (Either diff b) -> Automaton (ScheduleT diff m) a b
scheduleS = handleAutomaton $ \StreamT {state, step} ->
  let 
      step' s = ReaderT $ \a -> do
        Result s' eitherDiffB <- lift $ runReaderT (step s) a
        case eitherDiffB of
          Right b -> return $ Result s' b
          Left diff -> do
            wait diff
            runReaderT (step' s') a
  in StreamT
    { state
    , step = step'
    }

{-

Requirements: Each scheduled automaton must eventually produce an output or a diff greater than 'mempty'.
-}
instance (Show diff, Ord diff, TimeDifference diff, Monoid diff, Monad m, MonadSchedule m) => MonadSchedule (ScheduleT diff m ) where
  schedule automata = automata & N.zip [1..] & fmap instrument & schedule & backpressure & scheduleS & Automaton.concatS
    where
      nAutomata = List.length automata
      instrument :: (Int, Automaton (ScheduleT diff m) a b) -> Automaton m (a, diff) (Int, Either (Maybe diff) b)
      instrument (i, automaton) = flip runStateS__ mempty $ proc (a, globalTime) -> do
        localTime <- constM get -< ()

        if globalTime < localTime
          -- We are ahead of the consensus time, skip this tick instead of emitting
          -- FIXME how do I ensure that each input is consumed exactly once?
          then do
            returnA -< (i, Left Nothing)
          else do
            diffOrOutput <- liftS $ runScheduleS automaton -< a
            case diffOrOutput of
              Left diffNew -> do
                arrM $ modify -< (`add` diffNew)
              _ -> returnA -< ()
            returnA -< (i, Bifunctor.first Just diffOrOutput)

      -- FIXME Need to backpressure the total time, not the individual diffs! Because only one automaton will see it
      backpressure :: Automaton m (a, diff) (Int, Either (Maybe diff) b) -> Automaton m a (Either diff [b])
      backpressure scheduled = feedback (IM.fromAscList $ List.zip [1..nAutomata] $ List.repeat Seq.Empty, mempty) $ proc (a, (queues, lastGlobalTime)) -> do
        (i, outputOrDiffMaybe) <- scheduled -< (a, lastGlobalTime)
        let (output, queues') = popOutput $ enqueueOutput i outputOrDiffMaybe queues
        returnA -< (output, (queues', lastGlobalTime & either add (const id) output))

      enqueueOutput :: Int -> Either (Maybe diff) b -> IntMap (Seq (Either diff b)) -> IntMap (Seq (Either diff b))
      enqueueOutput i = \case
        Left Nothing -> id
        Left (Just diff) -> IM.insertWith (<>) i $ Seq.singleton $ Left diff
        Right b -> IM.insertWith (<>) i $ Seq.singleton $ Right b


      analyseQueue :: IntMap (Seq (Either diff b)) -> Maybe ( IntMap (Seq (Either diff b)), (IntMap diff, [b]))
      analyseQueue = let
        peekOutput i queuei = do
          case viewl queuei of
            -- FIXME Don't need to break if I have at least one output already
            -- Queue empty, break here and wait for more input
            EmptyL -> lift Nothing
            -- New diff enqueued.
            Left diff :< queuei' -> do
              modify $ \(diffs, bs) -> (IM.insert i diff diffs, bs)
              pure queuei'
            -- New output.
            Right b :< queuei' -> do
              modify $ \(diffs, bs) -> (diffs, b : bs)
              pure queuei'

        in flip runStateT (IM.empty, []) . IM.traverseWithKey peekOutput

      pushDiffsBack :: IntMap diff -> IntMap (Seq (Either diff b)) -> IntMap (Seq (Either diff b))
      pushDiffsBack diffs = IM.union $ diffs <&> pure . Left

      popOutput :: IntMap (Seq (Either diff b)) -> (Either diff [b], IntMap (Seq (Either diff b)))
      popOutput queues = case analyseQueue queues of
        Nothing -> (Right [], queues) -- Queues weren't full yet, need to wait for more input
        Just (queues', (diffs, bs)) -> case N.nonEmpty bs of
          Nothing -> if IM.null diffs
            then (Right [], queues') -- No diffs or outputs enqueued
            else
                let minDiff =  minimum diffs
                    adjustedDiffs = diffs <&?> \diff -> guard (diff /= minDiff) >> Just (diff `difference` minDiff)
              in (Left minDiff, pushDiffsBack adjustedDiffs queues')
          Just bs -> (Right $ toList bs, pushDiffsBack diffs queues')
      