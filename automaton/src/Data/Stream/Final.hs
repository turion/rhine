module Data.Stream.Final where

-- base
import Control.Applicative (Alternative (..))

-- mmorph
import Control.Monad.Morph (MFunctor (..))

-- automaton
import Data.Stream (StreamT (..), stepStream)
import Data.Stream.Result

{- | A stream transformer in final encoding.

One step of the stream transformer performs a monadic action and results in an output and a new stream.
-}
newtype Final m a = Final {getFinal :: m (Result (Final m a) a)}

{- | Translate an initially encoded stream into a finally encoded one.

This is usually a performance penalty.
-}
toFinal :: (Functor m) => StreamT m a -> Final m a
toFinal automaton = Final $ mapResultState toFinal <$> stepStream automaton
{-# INLINE toFinal #-}

{- | Translate a finally encoded stream into an initially encoded one.

The internal state is the stream itself.
-}
fromFinal :: Final m a -> StreamT m a
fromFinal final =
  StreamT
    { state = final
    , step = getFinal
    }
{-# INLINE fromFinal #-}

instance MFunctor Final where
  hoist morph = go
    where
      go Final {getFinal} = Final $ morph $ mapResultState go <$> getFinal

instance (Functor m) => Functor (Final m) where
  fmap f Final {getFinal} = Final $ fmap f . mapResultState (fmap f) <$> getFinal

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
