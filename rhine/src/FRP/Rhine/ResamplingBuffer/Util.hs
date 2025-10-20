{-# LANGUAGE RankNTypes #-}

{- |
Several utilities to create 'ResamplingBuffer's.
-}
module FRP.Rhine.ResamplingBuffer.Util where

-- transformers
import Control.Monad.Trans.Reader (runReaderT)

-- automaton
import Data.Stream (StreamT (..))
import Data.Stream.Internal (JointState (..))
import Data.Stream.Optimized (toStreamT)
import Data.Stream.Result (Result (..), mapResultState)

-- rhine
import FRP.Rhine.ClSF hiding (step, toStreamT)
import FRP.Rhine.Clock
import FRP.Rhine.ResamplingBuffer

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

infixl 4 &-&

-- | Parallely compose two 'ResamplingBuffer's, duplicating the input.
(&-&) ::
  Monad m =>
  ResamplingBuffer m cl1 cl2  a  b    ->
  ResamplingBuffer m cl1 cl2  a     c ->
  ResamplingBuffer m cl1 cl2  a (b, c)
resBuf1 &-& resBuf2 = arr (\a -> (a, a)) ^->> resBuf1 *-* resBuf2

{- | Given a 'ResamplingBuffer' where the output type depends on the input type polymorphically,
   we can produce a timestamped version that simply annotates every input value
   with the 'TimeInfo' when it arrived.
-}
timestamped ::
  Monad m =>
  (forall b. ResamplingBuffer m cl clf b (f b)) ->
  ResamplingBuffer m cl clf a (f (a, TimeInfo cl))
timestamped resBuf = (clId &&& timeInfo) ^->> resBuf
