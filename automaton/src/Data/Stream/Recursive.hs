{-# LANGUAGE RankNTypes #-}

module Data.Stream.Recursive where

-- base
import Control.Applicative (Alternative (..))
import Data.Function ((&))
import Data.Functor ((<&>))

-- mmorph
import Control.Monad.Morph (MFunctor (..))

-- automaton
import Data.Stream.Result

{- | A stream transformer in recursive encoding.

One step of the stream transformer performs a monadic action and results in an output and a new stream.
-}
newtype Recursive m a = Recursive {getRecursive :: m (Result (Recursive m a) a)}

instance MFunctor Recursive where
  hoist = hoist'

{- | Hoist a stream along a monad morphism, by applying said morphism to the step function.

This is like @mmorph@'s 'hoist', but it doesn't require a 'Monad' constraint on @m2@.
-}
hoist' :: (Functor f) => (forall x. f x -> g x) -> Recursive f a -> Recursive g a
hoist' morph = go
  where
    go Recursive {getRecursive} = Recursive $ morph $ mapResultState go <$> getRecursive

instance (Functor m) => Functor (Recursive m) where
  fmap f Recursive {getRecursive} = Recursive $ fmap f . mapResultState (fmap f) <$> getRecursive

instance (Applicative m) => Applicative (Recursive m) where
  pure a = go
    where
      go = Recursive $! pure $! Result go a

  Recursive mf <*> Recursive ma = Recursive $! (\(Result cf f) (Result ca a) -> Result (cf <*> ca) $! f a) <$> mf <*> ma

-- | Constantly perform the same effect, without remembering a state.
constM :: (Functor m) => m a -> Recursive m a
constM ma = go
  where
    go = Recursive $ Result go <$> ma

instance (Alternative m) => Alternative (Recursive m) where
  empty = constM empty

  Recursive ma1 <|> Recursive ma2 = Recursive $ ma1 <|> ma2

instance (Foldable m) => Foldable (Recursive m) where
  foldMap f Recursive {getRecursive} = foldMap (\(Result recursive a) -> f a <> foldMap f recursive) getRecursive

instance (Traversable m) => Traversable (Recursive m) where
  traverse f = go
    where
      go Recursive {getRecursive} = (getRecursive & traverse (\(Result cont a) -> flip Result <$> f a <*> go cont)) <&> Recursive

-- | Like 'fmap' or 'rmap', but the postcomposed function may have an effect in @m@.
mmap :: (Monad m) => (a -> m b) -> Recursive m a -> Recursive m b
mmap f Recursive {getRecursive} = Recursive $ do
  Result recursive a <- getRecursive
  b <- f a
  pure $ Result (mmap f recursive) b
