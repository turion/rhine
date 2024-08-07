module Data.Stream.Recursive where

-- base
import Control.Applicative (Alternative (..))

-- mmorph
import Control.Monad.Morph (MFunctor (..))

-- automaton
import Data.Stream (StreamT (..), stepStream)
import Data.Stream.Result

{- | A stream transformer in recursive encoding.

One step of the stream transformer performs a monadic action and results in an output and a new stream.
-}
newtype Recursive m a = Recursive {getRecursive :: m (Result (Recursive m a) a)}

{- | Translate a coalgebraicly encoded stream into a recursive one.

This is usually a performance penalty.
-}
toRecursive :: (Functor m) => StreamT m a -> Recursive m a
toRecursive automaton = Recursive $ mapResultState toRecursive <$> stepStream automaton
{-# INLINE toRecursive #-}

{- | Translate a recursive stream into a coalgebraicly encoded one.

The internal state is the stream itself.
-}
fromRecursive :: Recursive m a -> StreamT m a
fromRecursive coalgebraic =
  StreamT
    { state = coalgebraic
    , step = getRecursive
    }
{-# INLINE fromRecursive #-}

instance MFunctor Recursive where
  hoist morph = go
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
