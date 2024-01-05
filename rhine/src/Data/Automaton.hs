{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Automaton where

import Control.Applicative (Alternative (..), liftA2)
import Control.Monad.Morph (MFunctor (hoist))
import Data.Functor ((<&>))
import Data.Monoid (Ap (..))
import Prelude
import Data.VectorSpace (VectorSpace (..))

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
    Result s' f <- mf s
    Result s'' a <- ma s'
    pure (Result s'' (f a))

data AutomatonT m b where
  AutomatonT ::
    { state :: s
    , step :: s -> m (Result s b)
    } ->
    AutomatonT m b

-- FIXME Try whether Arr & ArrM constructors would give same performance benefits like rewrite rules

automatonT :: (Functor m) => s -> (s -> m (Result s b)) -> AutomatonT m b
automatonT = AutomatonT
{-# INLINE CONLIKE [1] automatonT #-}
{-# RULES
"automatonT @((), _)" forall s f.
  automatonT ((), s) f =
    AutomatonT s (\s1 -> f ((), s1) <&> \(Result ((), s2) b) -> Result s2 b)
"automatonT @(_, ())" forall s f.
  automatonT (s, ()) f =
    AutomatonT s (\s1 -> f (s1, ()) <&> \(Result (s2, ()) b) -> Result s2 b)
  #-}

instance (Functor m) => Functor (AutomatonT m) where
  fmap f AutomatonT {state, step} = automatonT state $! fmap (fmap f) <$> step
  {-# INLINE fmap #-}

stepAutomaton :: (Functor m) => AutomatonT m a -> m (Result (AutomatonT m a) a)
stepAutomaton AutomatonT {state, step} = mapResultState (`automatonT` step) <$> step state
{-# INLINE stepAutomaton #-}

data JointState a b = JointState a b

apResult :: Result s1 (a -> b) -> Result s2 a -> Result (JointState s1 s2) b
apResult (Result resultStateA outputF) (Result resultStateB outputA) = Result (JointState resultStateA resultStateB) $! outputF outputA
{-# INLINE apResult #-}

-- FIXME Use par & pseq in places
instance (Applicative m) => Applicative (AutomatonT m) where
  pure a =
    automatonT ()
      $! const
      $! pure
      $! Result
        { resultState = ()
        , output = a
        }
  {-# INLINE pure #-}

  AutomatonT stateF0 stepF <*> AutomatonT stateA0 stepA =
    automatonT (JointState stateF0 stateA0) (\(JointState stateF stateA) -> apResult <$> stepF stateF <*> stepA stateA)
  {-# INLINE (<*>) #-}

deriving via Ap (AutomatonT m) a instance (Applicative m, Num a) => Num (AutomatonT m a)

instance (Applicative m, Fractional a) => Fractional (AutomatonT m a) where
  fromRational = pure . fromRational
  recip = fmap recip

instance (Applicative m, Floating a) => Floating (AutomatonT m a) where
  pi = pure pi
  exp = fmap exp
  log = fmap log
  sin = fmap sin
  cos = fmap cos
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh

instance (VectorSpace v s, Eq s, Floating s, Applicative m) => VectorSpace (AutomatonT m v) (AutomatonT m s) where
  zeroVector = pure zeroVector
  (*^) = liftA2 (*^)
  (^+^) = liftA2 (^+^)
  dot = liftA2 dot
  normalize = fmap normalize

liftAutomaton :: (Functor m) => m b -> AutomatonT m b
liftAutomaton ma = automatonT () $! const $! Result () <$> ma
{-# INLINE liftAutomaton #-}

instance (Alternative m) => Alternative (AutomatonT m) where
  empty = liftAutomaton empty
  {-# INLINE empty #-}

  AutomatonT stateL0 stepL <|> AutomatonT stateR0 stepR =
    AutomatonT
      { state = JointState stateL0 stateR0
      , step = \(JointState stateL stateR) ->
          (mapResultState (`JointState` stateR) <$> stepL stateL)
            <|> (mapResultState (JointState stateL) <$> stepR stateR)
      }
  {-# INLINE (<|>) #-}

instance MFunctor AutomatonT where
  hoist f AutomatonT {state, step} = AutomatonT {state, step = f <$> step}
  {-# INLINE hoist #-}

-- FIXME rename to mapAutomaton?
morph :: (Functor n) => (forall s. m (Result s a) -> n (Result s b)) -> AutomatonT m a -> AutomatonT n b
morph f AutomatonT {state, step} = automatonT state $ fmap f step
{-# INLINE morph #-}

constM :: (Functor m) => m a -> AutomatonT m a
constM ma = AutomatonT () $! const $! Result () <$> ma
{-# INLINE constM #-}
