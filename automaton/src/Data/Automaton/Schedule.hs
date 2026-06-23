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
import Control.Monad (forM_, replicateM_)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity (..))
import Data.Bifunctor qualified as Bifunctor
import Data.Foldable1 (Foldable1 (toNonEmpty))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.NonEmpty as N
import Data.Maybe (isNothing, maybeToList)
import Data.Tuple (swap)

-- transformers
import Control.Monad.Trans.Accum (AccumT (..), runAccumT)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State.Strict (get, gets)
import Control.Monad.Trans.State.Strict qualified as State
import Control.Monad.Trans.Writer.CPS qualified as CPS
import Control.Monad.Trans.Writer.Lazy qualified as Lazy
import Control.Monad.Trans.Writer.Strict qualified as Strict

-- containers
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Set qualified as S

-- nonempty-containers
import Data.Sequence.NonEmpty qualified as NESeq

-- mmorph
import Control.Monad.Morph (MFunctor)

-- changeset
import Control.Monad.Trans.Changeset (ChangesetT (..))
import Data.Monoid.RightAction (RightAction)

-- time-domain
import Data.TimeDomain (TimeDifference (..))

-- automaton
import Data.Automaton (Automaton (..), arrM, constM, hoistS, initialised, liftS, mapMaybeS, reactimate, withAutomaton_)
import Data.Automaton qualified as Automaton
import Data.Automaton.Schedule.Trans (ScheduleT, runScheduleS, scheduleS)
import Data.Automaton.Trans.Except (exceptS)
import Data.Automaton.Trans.Maybe (maybeExit, runMaybeS)
import Data.Automaton.Trans.Reader (readerS, runReaderS)
import Data.Automaton.Trans.State (modify, runStateS__)
import Data.Stream (StreamT (..), concatS)
import Data.Stream.Optimized (OptimizedStreamT (Stateful), toStreamT)
import Data.Stream.Result (Result (..))

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
  {-# INLINE schedule #-}

instance (Monad m, MonadSchedule m) => MonadSchedule (ReaderT r m) where
  schedule =
    fmap runReaderS
      >>> schedule
      >>> readerS
  {-# INLINE schedule #-}

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
  {-# INLINE schedule #-}

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
  {-# INLINE schedule #-}

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
  {-# INLINE schedule #-}

instance (Monoid w, Monad m, MonadSchedule m) => MonadSchedule (CPS.WriterT w m) where
  schedule =
    fmap (withAutomaton_ (CPS.runWriterT >>> fmap (\(Result s a, w) -> Result s (a, w))))
      >>> schedule
      >>> withAutomaton_ (fmap (\(Result s (a, w)) -> (Result s a, w)) >>> CPS.writerT)
  {-# INLINE schedule #-}

instance (Monoid w, Monad m, MonadSchedule m) => MonadSchedule (Strict.WriterT w m) where
  schedule =
    fmap (withAutomaton_ (Strict.runWriterT >>> fmap (\(Result s a, w) -> Result s (a, w))))
      >>> schedule
      >>> withAutomaton_ (fmap (\(Result s (a, w)) -> (Result s a, w)) >>> Strict.WriterT)
  {-# INLINE schedule #-}

instance (Monoid w, Monad m, MonadSchedule m) => MonadSchedule (Lazy.WriterT w m) where
  schedule =
    fmap (withAutomaton_ (Lazy.runWriterT >>> fmap (\(Result s a, w) -> Result s (a, w))))
      >>> schedule
      >>> withAutomaton_ (fmap (\(Result s (a, w)) -> (Result s a, w)) >>> Lazy.WriterT)
  {-# INLINE schedule #-}

-- | This will share the accumulated log from the past with all automata
instance (Monoid w, Monad m, MonadSchedule m) => MonadSchedule (AccumT w m) where
  schedule =
    fmap (withAutomaton_ (runAccumT >>> ReaderT >>> CPS.writerT))
      >>> schedule
      >>> withAutomaton_ (CPS.runWriterT >>> runReaderT >>> AccumT)
  {-# INLINE schedule #-}

-- | This will share the accumulated state from the past with all automata
instance (Monoid w, RightAction w s, Monad m, MonadSchedule m) => MonadSchedule (ChangesetT s w m) where
  schedule =
    fmap (withAutomaton_ (getChangesetT >>> ReaderT >>> fmap swap >>> CPS.writerT))
      >>> schedule
      >>> withAutomaton_ (CPS.runWriterT >>> fmap swap >>> runReaderT >>> ChangesetT)
  {-# INLINE schedule #-}

{- | Cycle through all automata in a round-robin fashion.

On each tick of the combined 'Automaton', exactly one of the component
automata is stepped. The automata are advanced in the order they appear in
the input 'NonEmpty' list, cycling indefinitely.
-}
instance MonadSchedule Identity where
  schedule =
    fmap (getAutomaton >>> toStreamT)
      >>> roundRobinStreams
      >>> fmap N.toList
      >>> concatS
      >>> Stateful
      >>> Automaton
  {-# INLINE schedule #-}

{- | Step every substream in lock-step against the same input state and collect
the outputs into a 'NonEmpty' list, preserving input order.

The schedule state is a non-empty sequence of 'StreamT' cells.
Each tick advances all substreams via the underlying 'Applicative',
so any 'Applicative'-level effects (e.g. a 'WriterT' log) see the joint tick
as one atomic event rather than a per-substream sequence.
Downstream, 'concatS' flattens the @NonEmpty b@ into one @b@ per outer tick,
giving the round-robin semantics.
-}
roundRobinStreams :: (Applicative m) => NonEmpty (StreamT m b) -> StreamT m (NonEmpty b)
roundRobinStreams streams =
  StreamT
    { state = NESeq.fromList streams
    , step =
        fmap
          ( \stepped ->
              Result
                ((\(Result s' _) -> s') <$> stepped)
                (toNonEmpty ((\(Result _ b) -> b) <$> stepped))
          )
          . traverse stepOne
    }
  where
    stepOne (StreamT s f) = (\(Result s' b) -> Result (StreamT s' f) b) <$> f s
{-# INLINE roundRobinStreams #-}

-- | Each scheduled automaton must eventually produce an output or a diff greater than 'zero', otherwise this will loop indefinitely.
instance (Monoid diff, Ord diff, TimeDifference diff, Monad m, MonadSchedule m) => MonadSchedule (ScheduleT diff m) where
  schedule automata =
    automata
      & N.zip [1 ..]
      & fmap instrument -- substream per tick: (i, Maybe (diff, b))
      & schedule -- inner scheduler interleaves the substreams
      & backpressure -- buffer slots, gate by consensus, coalesce
      & scheduleS -- repack (diff, [b]) into ScheduleT diff
      & Automaton.concatS -- flatten [b] into one b per outer tick
    where
      nAutomata = N.length automata

      -- Per-substream wrapper. State = localTime (total diff consumed).
      -- Skips when ahead of consensus; otherwise steps once and bumps localTime.
      instrument ::
        (Int, Automaton (ScheduleT diff m) a b) ->
        Automaton m (a, diff) (Int, Maybe (diff, b))
      instrument (i, automaton) = flip runStateS__ mempty $ proc (a, globalTime) -> do
        localTime <- constM get -< ()
        if globalTime < localTime
          then returnA -< (i, Nothing) -- ahead of consensus, sit out
          else do
            (diff, b) <- liftS $ runScheduleS automaton -< a -- one (diff, b)
            arrM modify -< (`add` diff) -- localTime += diff
            returnA -< (i, Just (diff, b))

      -- Buffers per-substream slots, advances consensus when all slots are
      -- full, emits the min-diff substream's b (or several b's on ties).
      -- State = (slots, globalTime), threaded via `runStateS__` for
      -- consistency with `instrument`.
      backpressure ::
        Automaton m (a, diff) (Int, Maybe (diff, b)) ->
        Automaton m a (diff, [b])
      backpressure scheduled = flip runStateS__ (initialSlots, mempty) $ proc a -> do
        globalTime <- constM (gets snd) -< ()
        (i, maybeEmit) <- liftS scheduled -< (a, globalTime)
        -- Install slot-i's emission (when present). The skip-gate in
        -- `instrument` ensures we never overwrite an already-Just slot
        -- (queue depth stays ≤ 1).
        _ <- mapMaybeS (arrM (\(i', db) -> modify $ Bifunctor.first $ IM.insert i' (Just db))) -< (i,) <$> maybeEmit
        -- Pop a consensus tick atomically: read slots, compute popOutput,
        -- write back updated slots and advanced consensus, return (advance, bs).
        constM
          ( State.state $ \(slots, gt) ->
              let (advance, bs, slots') = popOutput slots
               in ((advance, bs), (slots', gt `add` advance))
          )
          -<
            ()
        where
          initialSlots :: IntMap (Maybe (diff, b))
          initialSlots = IM.fromAscList ((,Nothing) <$> [1 .. nAutomata])

      -- Emit a consensus tick when every slot is Just. Pop all b's whose
      -- diff equals the min, leaving the rest with their diff reduced by
      -- minDiff (they're still ahead of consensus by that delta).
      popOutput ::
        IntMap (Maybe (diff, b)) ->
        (diff, [b], IntMap (Maybe (diff, b)))
      popOutput slots
        | any isNothing (IM.elems slots) = (mempty, [], slots)
        | otherwise =
            let pairs = IM.mapMaybe id slots
                minDiff = minimum (fst <$> IM.elems pairs)
                stepSlot (d, b)
                  | d == minDiff = (Nothing, Just b)
                  | otherwise = (Just (d `difference` minDiff, b), Nothing)
                stepped = stepSlot <$> pairs
             in (minDiff, IM.elems (IM.mapMaybe snd stepped), fst <$> stepped)
  {-# INLINE schedule #-}
