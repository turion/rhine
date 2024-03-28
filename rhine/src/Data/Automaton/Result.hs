{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

module Data.Automaton.Result where

data Result s a = Result {resultState :: s, output :: ~a}
  deriving (Functor)

mapResultState :: (s1 -> s2) -> Result s1 a -> Result s2 a
mapResultState f Result {resultState, output} = Result {resultState = f resultState, output}
{-# INLINE mapResultState #-}

newtype ResultStateT s m a = ResultStateT {getResultStateT :: s -> m (Result s a)}
  deriving (Functor)

instance (Monad m) => Applicative (ResultStateT s m) where
  pure output = ResultStateT (\resultState -> pure Result {resultState, output})

  ResultStateT mf <*> ResultStateT ma = ResultStateT $ \s -> do
    Result !s' f <- mf s
    Result !s'' a <- ma s'
    pure (Result s'' (f a))
