{-# LANGUAGE RankNTypes #-}

{- |
Several utilities to create 'ResamplingBuffer's.
-}
module FRP.Rhine.ResamplingBuffer.Util where

-- base
import Data.Function ((&))

-- transformers
import Control.Monad.Trans.Reader (runReaderT)

-- time-domain
import Data.TimeDomain (TimeDomain (..))

-- automaton
import Data.Stream (StreamT (..))
import Data.Stream.Internal (JointState (..))
import Data.Stream.Optimized (toStreamT)
import Data.Stream.Result (Result (..), mapResultState)

-- rhine
import FRP.Rhine.ClSF hiding (step, toStreamT)
import FRP.Rhine.Clock
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.Schedule (ParallelClock)

-- * Utilities to build 'ResamplingBuffer's from smaller components

infix 2 >>-^

{- FOURMOLU_DISABLE -}

-- | Postcompose a 'ResamplingBuffer' with a matching 'ClSF'.
(>>-^) ::
  Monad m =>
  ResamplingBuffer m cl1 cl2 a b   ->
  ClSF             m     cl2   b c ->
  ResamplingBuffer m cl1 cl2 a   c
resbuf  >>-^ clsf = helper resbuf $ toStreamT $ getAutomaton clsf
  where
    helper ResamplingBuffer { buffer, put, get} StreamT { state, step} = ResamplingBuffer
      { buffer = JointState buffer state,
      put = \theTimeInfo a (JointState b s) -> (`JointState` s) <$> put theTimeInfo a b
      , get = \theTimeInfo (JointState b s) -> do
          Result b' b <- get theTimeInfo b
          Result s' c <- step s `runReaderT` b `runReaderT` theTimeInfo
          pure $! Result (JointState b' s') c
      }
{-# INLINE (>>-^) #-}

infix 1 ^->>

-- | Precompose a 'ResamplingBuffer' with a matching 'ClSF'.
(^->>) ::
  Monad m =>
  ClSF             m cl1     a b   ->
  ResamplingBuffer m cl1 cl2   b c ->
  ResamplingBuffer m cl1 cl2 a   c
clsf ^->> resBuf = helper (toStreamT (getAutomaton clsf)) resBuf
  where
   helper StreamT {state, step} ResamplingBuffer {buffer, put, get} = ResamplingBuffer
      {
        buffer = JointState buffer state
    , put = \theTimeInfo a (JointState buf s) -> do
      Result s' b <- step s `runReaderT` a `runReaderT` theTimeInfo
      buf' <- put theTimeInfo b buf
      pure $! JointState buf' s'
    , get = \theTimeInfo (JointState buf s) -> mapResultState (`JointState` s) <$> get theTimeInfo buf
      }
{-# INLINE (^->>) #-}

infixl 4 *-*

-- | Parallely compose two 'ResamplingBuffer's.
(*-*) ::
  Monad m =>
  ResamplingBuffer m cl1 cl2  a      b    ->
  ResamplingBuffer m cl1 cl2     c      d ->
  ResamplingBuffer m cl1 cl2 (a, c) (b, d)
ResamplingBuffer buf1 put1 get1 *-* ResamplingBuffer buf2 put2 get2 = ResamplingBuffer
  {
    buffer = JointState buf1 buf2
  , put = \theTimeInfo (a, c) (JointState s1 s2) -> do
      s1' <- put1 theTimeInfo a s1
      s2' <- put2 theTimeInfo c s2
      pure $! JointState s1' s2'
  , get = \theTimeInfo (JointState s1 s2) -> do
      Result s1' b <- get1 theTimeInfo s1
      Result s2' d <- get2 theTimeInfo s2
      pure $! Result (JointState s1' s2') (b, d)
  }
{-# INLINE (*-*) #-}

infixl 4 &-&

-- | Parallely compose two 'ResamplingBuffer's, duplicating the input.
(&-&) ::
  Monad m =>
  ResamplingBuffer m cl1 cl2  a  b    ->
  ResamplingBuffer m cl1 cl2  a     c ->
  ResamplingBuffer m cl1 cl2  a (b, c)
resBuf1 &-& resBuf2 = arr (\a -> (a, a)) ^->> resBuf1 *-* resBuf2
{-# INLINE (&-&) #-}

{- | Given a 'ResamplingBuffer' where the output type depends on the input type polymorphically,
   we can produce a timestamped version that simply annotates every input value
   with the 'TimeInfo' when it arrived.
-}
timestamped ::
  Monad m =>
  (forall b. ResamplingBuffer m cl clf b (f b)) ->
  ResamplingBuffer m cl clf a (f (a, TimeInfo cl))
timestamped resBuf = (clId &&& timeInfo) ^->> resBuf
{-# INLINE timestamped #-}

infixl 4 |-|

-- | Combine two 'ResamplingBuffer's in parallel input time.
--
-- The resulting 'ResamplingBuffer' will consume input whenever either of the input clocks ticks.
--
-- Caution: The time differences are split up between the two buffers, so the total passed time on the inputs is not the same as on the output.
(|-|) ::
  ( Monad m,
    TimeDomain (Time cl),
    Time clL ~ Time cl,
    Time clR ~ Time cl
  ) =>
  ResamplingBuffer m clL cl a b ->
  ResamplingBuffer m clR cl a c ->
  ResamplingBuffer m (ParallelClock clL clR) cl a (b, c)
ResamplingBuffer stateL putL getL |-| ResamplingBuffer stateR putR getR =
  ResamplingBuffer
    { buffer = JointState (JointState Nothing stateL) (JointState Nothing stateR),
      put = \theTimeInfo a (JointState (JointState lastTimeMaybeL sL) (JointState lastTimeMaybeR sR)) -> do
        let now = absolute theTimeInfo
        case tag theTimeInfo of
          Left tagL -> do
            sL' <- putL (theTimeInfo & retag (const tagL) & fixSinceLast lastTimeMaybeL) a sL
            pure $! JointState (JointState (Just now) sL') (JointState lastTimeMaybeR sR)
          Right tagR -> do
            sR' <- putR (theTimeInfo & retag (const tagR) & fixSinceLast lastTimeMaybeR) a sR
            pure $! JointState (JointState lastTimeMaybeL sL) (JointState (Just now) sR'),
      get = \theTimeInfo (JointState (JointState lastTimeMaybeL sL) (JointState lastTimeMaybeR sR)) -> do
        Result sL' b <- getL theTimeInfo sL
        Result sR' c <- getR theTimeInfo sR
        pure $! Result (JointState (JointState lastTimeMaybeL sL') (JointState lastTimeMaybeR sR')) (b, c)
    }
{-# INLINE (|-|) #-}

infixl 4 ||-||

-- | Combine two 'ResamplingBuffer's in parallel output time.
--
-- The resulting 'ResamplingBuffer' will produce output whenever either of the output clocks ticks.
--
-- Caution: The time differences are split up between the two buffers, so the total passed time on the input is not the same as on the outputs.
(||-||) ::
  ( Monad m,
    TimeDomain (Time cl),
    Time clL ~ Time cl,
    Time clR ~ Time cl
  ) =>
  ResamplingBuffer m cl                clL      a b ->
  ResamplingBuffer m cl                    clR  a b ->
  ResamplingBuffer m cl (ParallelClock clL clR) a b
ResamplingBuffer stateL putL getL ||-|| ResamplingBuffer stateR putR getR =
  ResamplingBuffer
    { buffer = JointState (JointState Nothing stateL) (JointState Nothing stateR),
      put = \theTimeInfo a (JointState (JointState lastTimeMaybeL sL) (JointState lastTimeMaybeR sR)) -> do
        sL' <- putL theTimeInfo a sL
        sR' <- putR theTimeInfo a sR
        pure $! JointState (JointState lastTimeMaybeL sL') (JointState lastTimeMaybeR sR'),
      get = \theTimeInfo (JointState (JointState lastTimeMaybeL sL) (JointState lastTimeMaybeR sR)) -> do
        let now = absolute theTimeInfo
        case tag theTimeInfo of
          Left tagL -> do
            Result sL' b <- getL (theTimeInfo & retag (const tagL) & fixSinceLast lastTimeMaybeL) sL
            pure $! Result (JointState (JointState (Just now) sL') (JointState lastTimeMaybeR sR)) b
          Right tagR -> do
            Result sR' b <- getR (theTimeInfo & retag (const tagR) & fixSinceLast lastTimeMaybeR) sR
            pure $! Result (JointState (JointState lastTimeMaybeL sL) (JointState (Just now) sR')) b
    }
{-# INLINE (||-||) #-}

-- | Helper function for 'ResamplingBuffer's over 'ParallelClock's to fix the 'sinceLast' field of the 'TimeInfo'.
fixSinceLast :: (TimeDomain (Time cl)) => Maybe (Time cl) -> TimeInfo cl -> TimeInfo cl
fixSinceLast lastTimeMaybe theTimeInfo = case lastTimeMaybe of
  Nothing -> theTimeInfo
  Just lastTime -> theTimeInfo {sinceLast = absolute theTimeInfo `diffTime` lastTime}
