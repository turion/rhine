module Data.Stream.Except where

-- base
import Control.Monad (ap)
import Data.Void

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

-- mmorph
import Control.Monad.Morph (MFunctor, hoist)

-- selective
import Control.Selective

-- automaton
import Data.Stream.Final (Final (..))
import Data.Stream.Final.Except
import Data.Stream.Optimized (OptimizedStreamT, applyExcept, constM, selectExcept)
import Data.Stream.Optimized qualified as StreamOptimized

{- | A stream that can terminate with an exception.

In @automaton@, such streams mainly serve as a vehicle to bring control flow to 'Data.Automaton.Trans.Except.AutomatonExcept'
(which is based on 'StreamExcept'), and the docs there apply here as well.

'StreamExcept' is not only a 'Monad', it also has more efficient 'Selective', 'Applicative', and 'Functor' interfaces.
-}
data StreamExcept a m e
  = -- | When using '>>=', this encoding will be used.
    FinalExcept (Final (ExceptT e m) a)
  | -- | This is usually the faster encoding, as it can be optimized by GHC.
    InitialExcept (OptimizedStreamT (ExceptT e m) a)

toFinal :: (Functor m) => StreamExcept a m e -> Final (ExceptT e m) a
toFinal (FinalExcept final) = final
toFinal (InitialExcept initial) = StreamOptimized.toFinal initial

runStreamExcept :: StreamExcept a m e -> OptimizedStreamT (ExceptT e m) a
runStreamExcept (FinalExcept final) = StreamOptimized.fromFinal final
runStreamExcept (InitialExcept initial) = initial

instance (Monad m) => Functor (StreamExcept a m) where
  fmap f (FinalExcept fe) = FinalExcept $ hoist (withExceptT f) fe
  fmap f (InitialExcept ae) = InitialExcept $ hoist (withExceptT f) ae

instance (Monad m) => Applicative (StreamExcept a m) where
  pure = InitialExcept . constM . throwE
  InitialExcept f <*> InitialExcept a = InitialExcept $ applyExcept f a
  f <*> a = ap f a

instance (Monad m) => Selective (StreamExcept a m) where
  select (InitialExcept e) (InitialExcept f) = InitialExcept $ selectExcept e f
  select e f = selectM e f

-- | 'return'/'pure' throw exceptions, '(>>=)' uses the last thrown exception as input for an exception handler.
instance (Monad m) => Monad (StreamExcept a m) where
  (>>) = (*>)
  ae >>= f = FinalExcept $ handleExceptT (toFinal ae) (toFinal . f)

instance MonadTrans (StreamExcept a) where
  lift = InitialExcept . constM . ExceptT . fmap Left

instance MFunctor (StreamExcept a) where
  hoist morph (InitialExcept automaton) = InitialExcept $ hoist (mapExceptT morph) automaton
  hoist morph (FinalExcept final) = FinalExcept $ hoist (mapExceptT morph) final

safely :: (Monad m) => StreamExcept a m Void -> OptimizedStreamT m a
safely = hoist (fmap (either absurd id) . runExceptT) . runStreamExcept
