module Data.Stream.Except where

-- base
import Control.Category ((>>>))
import Control.Monad (ap)
import Data.Bifunctor (bimap)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Void

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

-- mmorph
import Control.Monad.Morph (MFunctor, hoist)

-- selective
import Control.Selective

-- automaton
import Data.Stream (foreverExcept)
import Data.Stream.Optimized (OptimizedStreamT, applyExcept, constM, selectExcept)
import Data.Stream.Optimized qualified as StreamOptimized
import Data.Stream.Recursive (Recursive (..))
import Data.Stream.Recursive.Except
import Data.Stream.Result

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
mapOutput f (RecursiveExcept recursive) = RecursiveExcept $ f <$> recursive
mapOutput f (CoalgebraicExcept coalgebraic) = CoalgebraicExcept $ f <$> coalgebraic

toRecursive :: (Functor m) => StreamExcept a m e -> Recursive (ExceptT e m) a
toRecursive (RecursiveExcept recursive) = recursive
toRecursive (CoalgebraicExcept coalgebraic) = StreamOptimized.toRecursive coalgebraic

runStreamExcept :: StreamExcept a m e -> OptimizedStreamT (ExceptT e m) a
runStreamExcept (RecursiveExcept recursive) = StreamOptimized.fromRecursive recursive
runStreamExcept (CoalgebraicExcept coalgebraic) = coalgebraic

-- | Try to step the 'StreamExcept' for one value of the stream
stepInstant :: (Functor m) => StreamExcept a m e -> m (Either e (Result (StreamExcept a m e) a))
stepInstant (RecursiveExcept recursive) =
  recursive
    & getRecursive
    & runExceptT
    <&> fmap (mapResultState RecursiveExcept)
stepInstant (CoalgebraicExcept coalgebraic) =
  coalgebraic
    & StreamOptimized.stepOptimizedStream
    & runExceptT
    <&> fmap (mapResultState InitialExcept)

-- | Run all steps of the stream, discarding all output, until the exception is reached.
instance (Functor m, Foldable m) => Foldable (StreamExcept a m) where
  foldMap f = stepInstant >>> foldMap (either f $ resultState >>> foldMap f)

instance (Traversable m) => Traversable (StreamExcept a m) where
  traverse f streamExcept = traverseFinal (toFinal streamExcept) & fmap (Final >>> FinalExcept)
    where
      traverseFinal =
        getFinal
          >>> runExceptT
          >>> fmap ((bimap f $ mapResultState traverseFinal >>> (\Result {resultState, output} -> (Result <$> resultState) <&> ($ output))) >>> bitraverseEither)
          >>> traverse id
          >>> fmap (ExceptT >>> fmap (mapResultState Final))
      bitraverseEither :: (Functor f) => Either (f a) (f b) -> f (Either a b)
      bitraverseEither = either (fmap Left) (fmap Right)

-- FIXME This should work with Functor m and custom hoists
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

safe :: (Monad m) => OptimizedStreamT m a -> StreamExcept a m void
safe = CoalgebraicExcept . hoist lift

forever :: (Monad m) => StreamExcept a m e -> OptimizedStreamT m a
forever recursive@(RecursiveExcept _) = safely go
  where
    go = recursive >> go
forever (CoalgebraicExcept (StreamOptimized.Stateful stream)) = StreamOptimized.Stateful $ foreverExcept stream
forever (CoalgebraicExcept (StreamOptimized.Stateless f)) = StreamOptimized.Stateless go
  where
    go = runExceptT f >>= either (const go) return
