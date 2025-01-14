{-# LANGUAGE ExistentialQuantification #-}

module FRP.Rhine.Schedule.Internal where

-- base
import Control.Arrow
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

-- automaton
import Data.Stream hiding (concatS)
import Data.Stream.Result

-- | One step of a stream, with the state type argument going last, so it is usable with sop-core.
newtype Step m b state = Step {getStep :: ResultStateT state m b}

-- | The result of a stream, with the type arguments swapped, so it's usable with sop-core
newtype RunningResult b state = RunningResult {getRunningResult :: Result state b}

-- | Transform an n-ary product of at least one type into a nonempty list of all its content.
apInjs_NPNonEmpty :: (SListI xs) => NP f (x ': xs) -> NonEmpty (NS f (x ': xs))
apInjs_NPNonEmpty (fx :* fxs) = Z fx :| (S <$> apInjs_NP fxs)

-- | A nonempty list of 'StreamT's, unzipped into their states and their steps.
data Streams m b = forall state (states :: [Type]).
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
