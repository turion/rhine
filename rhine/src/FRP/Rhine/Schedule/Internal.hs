{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module FRP.Rhine.Schedule.Internal where

-- base
import Control.Arrow
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose (..))
import Data.Kind (Type)
import Data.List.NonEmpty as N

-- foldable1-classes-compat
import Data.Foldable1 (Foldable1 (foldrMap1))

-- sop-core
import Data.SOP (HCollapse (hcollapse), HSequence (htraverse'), I (..), K (K), NP (..), NS (..), SListI, apInjs_NP, hliftA, hzipWith, unI)

-- monad-schedule
import Control.Monad.Schedule.Class

-- transformers
import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT, writerT)

-- automaton
import Control.Monad.Morph (MFunctor (hoist))
import Data.Automaton (Automaton (..))
import Data.Stream hiding (concatS)
import Data.Stream.Optimized (OptimizedStreamT (..), toStreamT)
import Data.Stream.Result

-- | One step of a stream, with the state type argument going last, so it is usable with sop-core.
newtype Step m b state = Step {getStep :: ResultStateT state m b}

-- | The result of a stream, with the type arguments swapped, so it's usable with sop-core
newtype RunningResult b state = RunningResult {getRunningResult :: Result state b}

{- HLINT ignore apInjs_NPNonEmpty "Use camelCase" -}

-- | Transform an n-ary product of at least one type into a nonempty list of all its content.
apInjs_NPNonEmpty :: (SListI xs) => NP f (x ': xs) -> NonEmpty (NS f (x ': xs))
apInjs_NPNonEmpty (fx :* fxs) = Z fx :| (S <$> apInjs_NP fxs)

-- | A nonempty list of 'StreamT's, unzipped into their states and their steps.
data Streams m b
  = forall state (states :: [Type]).
  (SListI states) =>
  Streams
  { states :: NP I (state ': states)
  , steps :: NP (Step m b) (state ': states)
  }

-- | Run 'Streams' concurrently by scheduling them in 'MonadSchedule'.
scheduleStreams :: (MonadSchedule m, Functor m, Applicative m) => Streams m b -> StreamT m (NonEmpty b)
scheduleStreams Streams {states, steps} =
  StreamT
    { state = (apInjs_NPNonEmpty states, []) -- All the initial states and no currently running continuations
    , step =
        -- Some streams have not started yet, or just finished their step. Others are still running.
        \(restingStates, runningStreams) ->
          -- Start all currently not running streams by zipping each with its step
          fmap (htraverse' getCompose . hzipWith (\Step {getStep} -> Compose . fmap RunningResult . getResultStateT getStep . unI) steps) restingStates
            -- Append all already running states to the freshly started ones
            & flip appendList runningStreams
            -- Schedule all running streams concurrently
            & schedule
            -- Separate into finished streams and still running streams
            & fmap
              ( \(finished, running) ->
                  let finishedStates = finished <&> hliftA (getRunningResult >>> resultState >>> I)
                      outputs =
                        finished
                          <&> (hliftA (getRunningResult >>> output >>> K) >>> hcollapse)
                   in Result (finishedStates, running) outputs
              )
    }

-- | Run a nonempty list of streams concurrently.
scheduleStreams' :: (MonadSchedule m, Applicative m) => NonEmpty (StreamT m b) -> StreamT m (NonEmpty b)
scheduleStreams' = scheduleStreams . foldrMap1 buildStreams consStreams
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

schedIO' :: NonEmpty (StreamT IO a) -> StreamT IO a
schedIO' streams = initialising startStreams `comp` constM (ask >>= (lift . takeMVar))
  where
    startStreams = do
      var <- newEmptyMVar
      forM_ streams $ forkIO . reactimate . (`comp` constM (ask >>= (lift . putMVar var)))
      pure var

schedExcept' :: (Monad m) => (forall x. NonEmpty (StreamT m x) -> StreamT m x) -> NonEmpty (StreamT (ExceptT e m) a) -> StreamT (ExceptT e m) a
schedExcept' sched streams =
  streams
    <&> exceptS
    & sched
    & withStreamT (fmap sequenceA >>> ExceptT)

schedWriter :: (Monad m, Monoid w) => (forall x. NonEmpty (StreamT m x) -> StreamT m x) -> NonEmpty (StreamT (WriterT w m) a) -> StreamT (WriterT w m) a
schedWriter sched =
  fmap (withStreamT (runWriterT >>> fmap (\(Result s a, w) -> Result s (a, w))))
    >>> sched
    >>> withStreamT (fmap (\(Result s (a, w)) -> (Result s a, w)) >>> writerT)

schedAccum :: (Monad m, Monoid w) => (forall x. NonEmpty (StreamT m x) -> StreamT m x) -> NonEmpty (StreamT (AccumT w m) a) -> StreamT (AccumT w m) a
schedAccum sched = _

schedReader :: (Monad m) => (forall x. NonEmpty (StreamT m x) -> StreamT m x) -> NonEmpty (StreamT (ReaderT r m) a) -> StreamT (ReaderT r m) a
schedReader sched = fmap (withStreamT _) >>> _

initialising :: (Applicative m, Monad m) => m r -> StreamT m r
initialising action =
  let step mr@(Just r) = pure $! Result mr r
      step Nothing = (step . Just =<< action)
   in StreamT
        { state = Nothing
        , step
        }

comp :: (Monad m) => StreamT m r -> StreamT (ReaderT r m) a -> StreamT m a
comp smr srma = toStreamT $ hoist (`runReaderT` ()) $ getAutomaton $ Automaton (Stateful $ hoist lift smr) >>> Automaton (Stateful srma)
