{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Data.Stream.Result where

-- base
import Data.Bifunctor (Bifunctor (..))

-- automaton
import Data.Stream.Internal

{- | A tuple that is strict in its first argument.

This type is used in streams and automata to encode the result of a state transition.
The new state should always be strict to avoid space leaks.
-}
data Result s a = Result {resultState :: s, output :: ~a}
  deriving (Functor, Foldable, Traversable)

instance Bifunctor Result where
  second = fmap
  first = mapResultState

-- | Apply a function to the state of a 'Result'.
mapResultState :: (s1 -> s2) -> Result s1 a -> Result s2 a
mapResultState f Result {resultState, output} = Result {resultState = f resultState, output}
{-# INLINE mapResultState #-}

-- | Analogous to 'Applicative''s '(<*>)'.
apResult :: Result s1 (a -> b) -> Result s2 a -> Result (JointState s1 s2) b
apResult (Result resultStateA outputF) (Result resultStateB outputA) = Result (JointState resultStateA resultStateB) $ outputF outputA
{-# INLINE apResult #-}

-- | A state transformer with 'Result' instead of a standard tuple as its result.
newtype ResultStateT s m a = ResultStateT {getResultStateT :: s -> m (Result s a)}
  deriving (Functor)

instance (Monad m) => Applicative (ResultStateT s m) where
  pure output = ResultStateT (\resultState -> pure Result {resultState, output})

  ResultStateT mf <*> ResultStateT ma = ResultStateT $ \s -> do
    Result s' f <- mf s
    Result s'' a <- ma s'
    pure (Result s'' (f a))
