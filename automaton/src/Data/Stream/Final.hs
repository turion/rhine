module Data.Stream.Final where

-- base
import Control.Applicative (Alternative (..))

-- mmorph
import Control.Monad.Morph (MFunctor (..))

-- automaton
import Data.Stream.Result
import Data.Function ((&))
import Data.Functor ((<&>))

{- | A stream transformer in final encoding.

One step of the stream transformer performs a monadic action and results in an output and a new stream.
-}
newtype Final m a = Final {getFinal :: m (Result (Final m a) a)}

instance MFunctor Final where
  hoist = hoist'

hoist' morph = go
  where
    go Final {getFinal} = Final $ morph $ mapResultState go <$> getFinal

instance (Functor m) => Functor (Final m) where
  fmap f Final {getFinal} = Final $ fmap f . mapResultState (fmap f) <$> getFinal

-- FIXME define for automaton as well
-- FIXME go trick
mmap :: Monad m => (a -> m b) -> Final m a -> Final m b
mmap f Final {getFinal} = Final $ do
  Result final a <- getFinal
  b <- f a
  return $ Result (mmap f final) b

instance (Applicative m) => Applicative (Final m) where
  pure a = go
    where
      go = Final $! pure $! Result go a

  Final mf <*> Final ma = Final $! (\(Result cf f) (Result ca a) -> Result (cf <*> ca) $! f a) <$> mf <*> ma

-- | Constantly perform the same effect, without remembering a state.
constM :: (Functor m) => m a -> Final m a
constM ma = go
  where
    go = Final $ Result go <$> ma

instance (Alternative m) => Alternative (Final m) where
  empty = constM empty

  Final ma1 <|> Final ma2 = Final $ ma1 <|> ma2

instance Foldable m => Foldable (Final m) where
  foldMap f Final {getFinal} = foldMap (\(Result final a) -> f a <> foldMap f final) getFinal

instance (Traversable m) => Traversable (Final m) where
  traverse f = go
    where
      go Final {getFinal} = (getFinal & traverse (\(Result cont a) -> flip Result <$> f a <*> go cont)) <&> Final
