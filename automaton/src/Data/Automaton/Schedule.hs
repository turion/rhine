{-# LANGUAGE OverloadedLists #-}

{- | -
This module defines the 'MonadSchedule' class for running several automata concurrently,
and provides instances for common monad transformers.

The central abstraction is:

@'schedule' :: 'NonEmpty' ('Automaton' m a b) -> 'Automaton' m a b@

This takes a non-empty collection of automata and interleaves their outputs,
yielding one output per tick by cycling through the automata in some
monad-specific order (e.g. round-robin for 'Identity', first-available for 'IO').
For a free simulated-time scheduling monad transformer, see 'ScheduleT' in "Data.Automaton.Schedule.Trans".

== Relationship to @monad-schedule@

This class replaces the @MonadSchedule@ class from the @monad-schedule@ package
(which worked on monadic actions @m a@ directly) with one that works natively on 'Automaton' values.
The free waiting effect 'ScheduleT' from the @monad-schedule@ has been moved to @automaton@.
-}
module Data.Automaton.Schedule where

-- base
import Control.Arrow
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, guard, replicateM_)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity (..))
import Data.Bifunctor qualified as Bifunctor
import Data.Foldable1 (Foldable1 (foldrMap1))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose (..))
import Data.Kind (Type)
import Data.List qualified as List
import Data.List.NonEmpty as N
import Data.Maybe (maybeToList)
import Data.Tuple (swap)

-- transformers
import Control.Monad.Trans.Accum (AccumT (..), runAccumT)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State.Strict (StateT (..), get)
import Control.Monad.Trans.Writer.CPS qualified as CPS
import Control.Monad.Trans.Writer.Lazy qualified as Lazy
import Control.Monad.Trans.Writer.Strict qualified as Strict

-- sop-core
import Data.SOP (HCollapse (hcollapse), HSequence (htraverse'), I (..), K (..), NP (..), SListI, hmap, hzipWith)

-- containers
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Sequence (Seq, ViewL (..), viewl)
import Data.Sequence qualified as Seq
import Data.Set qualified as S

-- mmorph
import Control.Monad.Morph (MFunctor)

-- witherable
import Witherable ((<&?>))

-- changeset
import Control.Monad.Trans.Changeset (ChangesetT (..))
import Data.Monoid.RightAction (RightAction)

-- time-domain
import Data.TimeDomain (TimeDifference (..))

-- automaton
import Data.Automaton (Automaton (..), arrM, constM, feedback, hoistS, initialised, liftS, reactimate, withAutomaton_)
import Data.Automaton qualified as Automaton
import Data.Automaton.Schedule.Trans (ScheduleT, SkipT, runScheduleS, runSkipS, scheduleS)
import Data.Automaton.Trans.Except (exceptS)
import Data.Automaton.Trans.Maybe (maybeExit, runMaybeS)
import Data.Automaton.Trans.Reader (readerS, runReaderS)
import Data.Automaton.Trans.State (modify, runStateS__)
import Data.Stream (StreamT (..), concatS)
import Data.Stream.Optimized (OptimizedStreamT (Stateful), toStreamT)
import Data.Stream.Result

{- | Class of monads that support running several 'Automaton's concurrently,
interleaving their outputs into a single 'Automaton'.

The semantics of 'schedule' depend on the monad:

* For 'Identity': round-robin (each automaton advances exactly once per cycle).
* For 'IO': all automata run in separate threads; results are delivered as soon
 as they are produced.
* For transformer stacks: defined compositionally by the individual instances.

The first input may be broadcast to an arbitrary number (1 for 'Identity', all for 'IO') of automata,
but subsequent inputs must be delivered to one automaton each.
-}
class MonadSchedule m where
  -- | Run a nonempty list of automata concurrently.
  schedule :: NonEmpty (Automaton m a b) -> Automaton m a b

{- | Start all streams in the background and send their values to a shared 'MVar'.

The first input is broadcast to all automata,
the following inputs are only broadcast to one each.
-}
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

{- | Schedule automata in 'ExceptT'.

When any automaton throws an exception, all others are stopped immediately.
This is intentional: a thrown exception signals that the computation cannot
continue, so it is consistent to stop all peers.

To let all automata run to completion before propagating an exception, lift
them into a monad that does not short-circuit on exceptions (e.g. wrap the
exception type in 'Either' and post-process the results).
-}
instance (Monad m, MonadSchedule m) => MonadSchedule (ExceptT e m) where
  schedule =
    fmap exceptS
      >>> schedule
      >>> withAutomaton_ (fmap sequenceA >>> ExceptT)

{- | Schedule automata in 'MaybeT'.

When any automaton returns 'Nothing', all others are stopped immediately and the
combined automaton also returns 'Nothing'. This is intentional: 'Nothing'
signals termination, so it is consistent to stop all peers.

To let all automata run to completion before stopping, convert the 'MaybeT'
automata to base-monad automata producing @'Maybe' b@ values and post-process
the results.
-}
instance (Monad m, MonadSchedule m) => MonadSchedule (MaybeT m) where
  schedule =
    fmap runMaybeS
      >>> schedule
      >>> withAutomaton_ (fmap sequenceA >>> MaybeT)

{- | A monad transformer for scheduling automata that all need to run to
completion.

Like 'MaybeT', each automaton can signal termination by returning 'Nothing'.
Unlike 'MaybeT', the combined automaton does __not__ stop when one automaton
finishes; it waits until __all__ scheduled automata have finished.

While any automaton is still running, finished automata contribute no outputs.
Once every automaton has finished, the combined automaton itself terminates.
-}
newtype FinalizeT m a = FinalizeT
  { getFinalizeT :: MaybeT m a
  -- ^ Unwrap 'FinalizeT' to the underlying 'MaybeT'.
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans, MFunctor)

instance (Monad m, MonadSchedule m) => MonadSchedule (FinalizeT m) where
  schedule automata =
    automata
      & N.zip [1 ..]
      & fmap (\(i, automaton) -> runMaybeS (hoistS getFinalizeT automaton) <&> maybe (Left i) Right)
      & schedule
      & (>>> haveAllFinished)
      & liftS
      & (>>> maybeExit)
      & fmap maybeToList
      & Automaton.concatS
      & hoistS FinalizeT
    where
      allN = S.fromAscList [1 .. N.length automata]
      haveAllFinished = Automaton.unfold S.empty $ \input is -> case input of
        Left i -> let is' = S.insert i is in Result is' $ if is' == allN then Nothing else Just Nothing
        Right b -> Result is $ Just $ Just b

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

{- | Cycle through all automata in a round-robin fashion.

On each tick of the combined 'Automaton', exactly one of the component
automata is stepped. The automata are advanced in the order they appear in
the input 'NonEmpty' list, cycling indefinitely.
-}
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

-- The order of outputs matches the order of inputs: 'foldrMap1' places the
-- first input element at the head of the 'NP', so 'hnonemptycollapse' extracts
-- outputs in the original order.

{- | Step all streams in a 'Streams' bundle simultaneously and collect the
results into a 'NonEmpty' list, preserving the input order.
-}
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

-- | Collapse a non-empty n-ary product of constant functors into a 'NonEmpty' list.
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

instance (Monad m, MonadSchedule m) => MonadSchedule (SkipT m) where
  schedule = fmap runSkipS >>> schedule >>> fmap maybeToList >>> Automaton.concatS >>> liftS

-- | Each scheduled automaton must eventually produce an output or a diff greater than 'zero', otherwise this will loop indefinitely.
instance (Ord diff, TimeDifference diff, Monad m, MonadSchedule m) => MonadSchedule (ScheduleT diff m) where
  schedule automata = automata & N.zip [1 ..] & fmap instrument & schedule & backpressure & scheduleS & Automaton.concatS
    where
      nAutomata = List.length automata
      instrument :: (Int, Automaton (ScheduleT diff m) a b) -> Automaton m (a, diff) (Int, Either (Maybe diff) b)
      instrument (i, automaton) = flip runStateS__ mempty $ proc (a, globalTime) -> do
        localTime <- constM get -< ()

        if globalTime < localTime
          -- We are ahead of the consensus time, skip this tick instead of emitting
          -- Each automaton ticks once per round-robin cycle over all N automata,
          -- so each input 'a' is delivered to each automaton exactly once per cycle.
          then do
            returnA -< (i, Left Nothing)
          else do
            diffOrOutput <- liftS $ runScheduleS automaton -< a
            case diffOrOutput of
              Left diffNew -> do
                arrM modify -< (`add` diffNew)
              _ -> returnA -< ()
            returnA -< (i, Bifunctor.first Just diffOrOutput)

      -- Each tick of 'schedule' advances exactly one of the N instrumented automata.
      -- 'backpressure' feeds the current consensus time back as the second input
      -- so that automata which are ahead of the consensus will skip their tick.
      backpressure :: Automaton m (a, diff) (Int, Either (Maybe diff) b) -> Automaton m a (Either diff [b])
      backpressure scheduled = feedback (IM.fromAscList $ (,Seq.Empty) <$> [1 .. nAutomata], mempty) $ proc (a, (queues, lastGlobalTime)) -> do
        (i, outputOrDiffMaybe) <- scheduled -< (a, lastGlobalTime)
        let (output, queues') = popOutput $ enqueueOutput i outputOrDiffMaybe queues
        returnA -< (output, (queues', lastGlobalTime & either add (const id) output))

      enqueueOutput :: Int -> Either (Maybe diff) b -> IntMap (Seq (Either diff b)) -> IntMap (Seq (Either diff b))
      enqueueOutput i = \case
        Left Nothing -> id
        Left (Just diff) -> IM.insertWith (<>) i $ Seq.singleton $ Left diff
        Right b -> IM.insertWith (<>) i $ Seq.singleton $ Right b

      analyseQueue :: IntMap (Seq (Either diff b)) -> Maybe (IntMap (Seq (Either diff b)), (IntMap diff, [b]))
      analyseQueue =
        let peekOutput i queuei = do
              case viewl queuei of
                -- Queue empty: we cannot yet determine a consensus output for this
                -- automaton, so we abort and wait for more input from 'schedule'.
                -- This conservatively waits even if some other automata already
                -- have outputs queued; a more aggressive variant could return
                -- those partial results early.
                EmptyL -> lift Nothing
                -- New diff enqueued.
                Left diff :< queuei' -> do
                  modify $ Bifunctor.first $ IM.insert i diff
                  pure queuei'
                -- New output.
                Right b :< queuei' -> do
                  modify $ Bifunctor.second (b :)
                  pure queuei'
         in flip runStateT (IM.empty, []) . IM.traverseWithKey peekOutput

      pushDiffsBack :: IntMap diff -> IntMap (Seq (Either diff b)) -> IntMap (Seq (Either diff b))
      pushDiffsBack diffs = IM.unionWith (<>) $ diffs <&> pure . Left

      popOutput :: IntMap (Seq (Either diff b)) -> (Either diff [b], IntMap (Seq (Either diff b)))
      popOutput queues = case analyseQueue queues of
        Nothing -> (Right [], queues) -- Queues weren't full yet, need to wait for more input
        Just (queues', (diffs, bs)) -> case N.nonEmpty bs of
          Nothing ->
            if IM.null diffs
              then (Right [], queues') -- No diffs or outputs enqueued
              else
                let minDiff = minimum diffs
                    adjustedDiffs = diffs <&?> \diff -> guard (diff /= minDiff) >> Just (diff `difference` minDiff)
                 in (Left minDiff, pushDiffsBack adjustedDiffs queues')
          Just bs -> (Right $ List.reverse $ toList bs, pushDiffsBack diffs queues')
