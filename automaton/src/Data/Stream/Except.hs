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
import Data.Stream.Optimized (OptimizedStreamT, applyExcept, constM, selectExcept)
import Data.Stream.Optimized qualified as StreamOptimized
import Data.Stream.Recursive (Recursive (..))
import Data.Stream.Recursive.Except

{- | A stream that can terminate with an exception.

In @automaton@, such streams mainly serve as a vehicle to bring control flow to 'Data.Automaton.Trans.Except.AutomatonExcept'
(which is based on 'StreamExcept'), and the docs there apply here as well.

'StreamExcept' is not only a 'Monad', it also has more efficient 'Selective', 'Applicative', and 'Functor' interfaces.
-}
data StreamExcept a m e
  = -- | When using '>>=', this encoding will be used.
    RecursiveExcept (Recursive (ExceptT e m) a)
  | -- | This is usually the faster encoding, as it can be optimized by GHC.
    CoalgebraicExcept (OptimizedStreamT (ExceptT e m) a)

-- | Apply a function to the output of the stream
mapOutput :: (Functor m) => (a -> b) -> StreamExcept a m e -> StreamExcept b m e
mapOutput f (RecursiveExcept final) = RecursiveExcept $ f <$> final
mapOutput f (CoalgebraicExcept initial) = CoalgebraicExcept $ f <$> initial

toRecursive :: (Functor m) => StreamExcept a m e -> Recursive (ExceptT e m) a
toRecursive (RecursiveExcept coalgebraic) = coalgebraic
toRecursive (CoalgebraicExcept coalgebraic) = StreamOptimized.toRecursive coalgebraic

runStreamExcept :: StreamExcept a m e -> OptimizedStreamT (ExceptT e m) a
runStreamExcept (RecursiveExcept coalgebraic) = StreamOptimized.fromRecursive coalgebraic
runStreamExcept (CoalgebraicExcept coalgebraic) = coalgebraic

instance (Monad m) => Functor (StreamExcept a m) where
  fmap f (RecursiveExcept fe) = RecursiveExcept $ hoist (withExceptT f) fe
  fmap f (CoalgebraicExcept ae) = CoalgebraicExcept $ hoist (withExceptT f) ae

instance (Monad m) => Applicative (StreamExcept a m) where
  pure = CoalgebraicExcept . constM . throwE
  CoalgebraicExcept f <*> CoalgebraicExcept a = CoalgebraicExcept $ applyExcept f a
  f <*> a = ap f a

instance (Monad m) => Selective (StreamExcept a m) where
  select (CoalgebraicExcept e) (CoalgebraicExcept f) = CoalgebraicExcept $ selectExcept e f
  select e f = selectM e f

-- | 'return'/'pure' throw exceptions, '(>>=)' uses the last thrown exception as input for an exception handler.
instance (Monad m) => Monad (StreamExcept a m) where
  (>>) = (*>)
  ae >>= f = RecursiveExcept $ handleExceptT (toRecursive ae) (toRecursive . f)

instance MonadTrans (StreamExcept a) where
  lift = CoalgebraicExcept . constM . ExceptT . fmap Left

instance MFunctor (StreamExcept a) where
  hoist morph (RecursiveExcept recursive) = RecursiveExcept $ hoist (mapExceptT morph) recursive
  hoist morph (CoalgebraicExcept coalgebraic) = CoalgebraicExcept $ hoist (mapExceptT morph) coalgebraic

safely :: (Monad m) => StreamExcept a m Void -> OptimizedStreamT m a
safely = hoist (fmap (either absurd id) . runExceptT) . runStreamExcept
