{-# LANGUAGE LambdaCase #-}
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
import Control.Monad.Trans.Reader (ReaderT (..))

-- mmorph
import Control.Monad.Morph (MFunctor, hoist)

-- selective
import Control.Selective

-- automaton
import Data.Stream (foreverExcept, foreverExceptE)
import Data.Stream.Optimized as OptimizedStreamT (OptimizedStreamT, applyExcept, constM, hoist', selectExcept)
import Data.Stream.Optimized qualified as StreamOptimized
import Data.Stream.Recursive (Recursive (..))
import Data.Stream.Recursive as Recursive (hoist')
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

stepInstant :: (Functor m) => StreamExcept a m e -> m (Either e (Result (StreamExcept a m e) a))
stepInstant = runStreamExcept >>> StreamOptimized.stepOptimizedStream >>> runExceptT >>> fmap (fmap (mapResultState CoalgebraicExcept))

-- | Run all steps of the stream, discarding all output, until the exception is reached.
instance (Functor m, Foldable m) => Foldable (StreamExcept a m) where
  foldMap f = stepInstant >>> foldMap (either f $ resultState >>> foldMap f)

instance (Traversable m) => Traversable (StreamExcept a m) where
  traverse f streamExcept = traverseRecursive (toRecursive streamExcept) & fmap (Recursive >>> RecursiveExcept)
    where
      traverseRecursive =
        getRecursive
          >>> runExceptT
          >>> fmap (bimap f (mapResultState traverseRecursive >>> (\Result {resultState, output} -> (Result <$> resultState) <&> ($ output))) >>> bitraverseEither)
          >>> sequenceA
          >>> fmap (ExceptT >>> fmap (mapResultState Recursive))
      bitraverseEither :: (Functor f) => Either (f a) (f b) -> f (Either a b)
      bitraverseEither = either (fmap Left) (fmap Right)

instance (Functor m) => Functor (StreamExcept a m) where
  fmap f (RecursiveExcept fe) = RecursiveExcept $ Recursive.hoist' (withExceptT f) fe
  fmap f (CoalgebraicExcept ae) = CoalgebraicExcept $ OptimizedStreamT.hoist' (withExceptT f) ae
  {-# INLINABLE fmap #-}

instance (Monad m) => Applicative (StreamExcept a m) where
  pure = CoalgebraicExcept . constM . throwE
  {-# INLINABLE pure #-}
  CoalgebraicExcept f <*> CoalgebraicExcept a = CoalgebraicExcept $ applyExcept f a
  f <*> a = ap f a
  {-# INLINABLE (<*>) #-}

instance (Monad m) => Selective (StreamExcept a m) where
  select (CoalgebraicExcept e) (CoalgebraicExcept f) = CoalgebraicExcept $ selectExcept e f
  select e f = selectM e f
  {-# INLINABLE select #-}

-- | 'return'/'pure' throw exceptions, '(>>=)' uses the last thrown exception as input for an exception handler.
instance (Monad m) => Monad (StreamExcept a m) where
  (>>) = (*>)
  {-# INLINE (>>) #-} -- FIXME this doesn't inline properly. Because of polymorphism?
  ae >>= f = RecursiveExcept $ handleExceptT (toRecursive ae) (toRecursive . f)

instance MonadTrans (StreamExcept a) where
  lift = CoalgebraicExcept . constM . ExceptT . fmap Left
  {-# INLINABLE lift #-}

instance MFunctor (StreamExcept a) where
  hoist morph (RecursiveExcept recursive) = RecursiveExcept $ hoist (mapExceptT morph) recursive
  hoist morph (CoalgebraicExcept coalgebraic) = CoalgebraicExcept $ hoist (mapExceptT morph) coalgebraic
  {-# INLINABLE hoist #-}

{- | If no exception can occur, the stream can be executed without the 'ExceptT'
layer.

Used to exit the 'StreamExcept' context, often in combination with 'safe'.
-}
safely :: (Monad m) => StreamExcept a m Void -> OptimizedStreamT m a
safely = hoist (fmap (either absurd id) . runExceptT) . runStreamExcept
{-# INLINABLE safely #-}

{- | A stream without an 'ExceptT' layer never throws an exception,
and can thus have an arbitrary exception type.

In particular, the exception type can be 'Void', so it can be used as the last statement in a 'StreamExcept' @do@-block.
See 'safely' for an example.
-}
safe :: (Monad m) => OptimizedStreamT m a -> StreamExcept a m void
safe = CoalgebraicExcept . hoist lift

{- | Run the stream until the exception is thrown, then restart, continuing this cycle forever.
-}
forever :: (Monad m) => StreamExcept a m e -> OptimizedStreamT m a
forever recursive@(RecursiveExcept _) = safely go
  where
    go = recursive >> go
forever (CoalgebraicExcept (StreamOptimized.Stateful stream)) = StreamOptimized.Stateful $ foreverExcept stream
forever (CoalgebraicExcept (StreamOptimized.Stateless f)) = StreamOptimized.Stateless go
  where
    go = runExceptT f >>= either (const go) pure

{- | Like 'forever', but keep the last thrown exception.

Before any exception was thrown, an initialisation value is given.
-}
foreverE :: (Monad m) =>
  -- | The initial value that is supplied to the 'ReaderT' context before the first exception is thrown
  e -> StreamExcept a (ReaderT e m) e -> OptimizedStreamT m a
foreverE e = \case
  recursive@(RecursiveExcept _) -> safely $ go e
    where
      go e = hoist (`runReaderT` e) recursive >>= go
  (CoalgebraicExcept (StreamOptimized.Stateful stream)) -> StreamOptimized.Stateful $ foreverExceptE e stream
  (CoalgebraicExcept (StreamOptimized.Stateless f)) -> StreamOptimized.Stateless $ go e
    where
      go e = runReaderT (runExceptT f) e >>= either go pure
