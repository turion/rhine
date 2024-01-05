{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Automaton.Optimized where

-- base
import Control.Applicative (Alternative (..), liftA2)
import Data.Monoid (Ap (..))

-- transformers
import Control.Monad.Trans.Except (ExceptT)

-- simple-affine-space
import Data.VectorSpace

-- mmorph
import Control.Monad.Morph

-- rhine

import Control.Monad.Trans.Except (ExceptT)
import Data.Automaton
import Data.Automaton qualified as AutomatonT
import Data.Automaton.Final (Final (..))
import Data.Automaton.Final qualified as Final (fromFinal, toFinal)
import Data.Automaton.Result

data OptimizedAutomatonT m a
  = Stateful (AutomatonT m a)
  | Stateless (m a)
  deriving (Functor)

toAutomatonT :: (Functor m) => OptimizedAutomatonT m b -> AutomatonT m b
toAutomatonT (Stateful automaton) = automaton
toAutomatonT (Stateless m) = AutomatonT {state = (), step = const $ Result () <$> m}

instance (Applicative m) => Applicative (OptimizedAutomatonT m) where
  pure = Stateless . pure
  {-# INLINE pure #-}

  Stateful automaton1 <*> Stateful automaton2 = Stateful $ automaton1 <*> automaton2
  Stateless m <*> Stateful (AutomatonT state0 step) = Stateful $ AutomatonT state0 $ \state -> fmap . ($) <$> m <*> step state
  Stateful (AutomatonT state0 step) <*> Stateless m = Stateful $ AutomatonT state0 $ \state -> flip (fmap . flip ($)) <$> step state <*> m
  Stateless mf <*> Stateless ma = Stateless $ mf <*> ma
  {-# INLINE (<*>) #-}

deriving via Ap (OptimizedAutomatonT m) a instance (Applicative m, Num a) => Num (OptimizedAutomatonT m a)

instance (Applicative m, Fractional a) => Fractional (OptimizedAutomatonT m a) where
  fromRational = pure . fromRational
  recip = fmap recip

instance (Applicative m, Floating a) => Floating (OptimizedAutomatonT m a) where
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

instance (VectorSpace v s, Eq s, Floating s, Applicative m) => VectorSpace (OptimizedAutomatonT m v) (OptimizedAutomatonT m s) where
  zeroVector = pure zeroVector
  (*^) = liftA2 (*^)
  (^+^) = liftA2 (^+^)
  dot = liftA2 dot
  normalize = fmap normalize

instance (Alternative m) => Alternative (OptimizedAutomatonT m) where
  empty = Stateless empty
  {-# INLINE empty #-}

  -- The semantics prescribe that we save the state which automaton was selected.
  automaton1 <|> automaton2 = Stateful $ toAutomatonT automaton1 <|> toAutomatonT automaton2
  {-# INLINE (<|>) #-}

instance MFunctor OptimizedAutomatonT where
  hoist f (Stateful automaton) = Stateful $ hoist f automaton
  hoist f (Stateless m) = Stateless $ f m
  {-# INLINE hoist #-}

mapOptimizedAutomatonT :: (Functor m, Functor n) => (forall s. m (Result s a) -> n (Result s b)) -> OptimizedAutomatonT m a -> OptimizedAutomatonT n b
mapOptimizedAutomatonT f (Stateful automaton) = Stateful $ mapAutomatonT f automaton
mapOptimizedAutomatonT f (Stateless m) = Stateless $ fmap output $ f $ fmap (Result ()) m

mapS :: (Monad m) => (forall m. (Monad m) => AutomatonT m a -> AutomatonT m b) -> OptimizedAutomatonT m a -> OptimizedAutomatonT m b
mapS f automaton = Stateful $ f $ toAutomatonT automaton

handleS :: (Functor m) => (AutomatonT m a -> AutomatonT n b) -> OptimizedAutomatonT m a -> OptimizedAutomatonT n b
handleS f automaton = Stateful $ f $ toAutomatonT automaton

reactimate :: (Monad m) => OptimizedAutomatonT m () -> m void
reactimate (Stateful automaton) = AutomatonT.reactimate automaton
reactimate (Stateless f) = go
  where
    go = f *> go

constM :: m a -> OptimizedAutomatonT m a
constM = Stateless

stepOptimizedAutomaton :: (Functor m) => OptimizedAutomatonT m a -> m (Result (OptimizedAutomatonT m a) a)
stepOptimizedAutomaton (Stateful automaton) = mapResultState Stateful <$> stepAutomaton automaton
stepOptimizedAutomaton oa@(Stateless m) = Result oa <$> m

toFinal :: (Functor m) => OptimizedAutomatonT m a -> Final m a
toFinal (Stateful automaton) = Final.toFinal automaton
toFinal (Stateless f) = go
  where
    go = Final $ Result go <$> f

fromFinal :: Final m a -> OptimizedAutomatonT m a
fromFinal = Stateful . Final.fromFinal

concatS :: (Monad m) => OptimizedAutomatonT m [a] -> OptimizedAutomatonT m a
concatS automaton = Stateful $ AutomatonT.concatS $ toAutomatonT automaton

exceptS :: (Monad m) => OptimizedAutomatonT (ExceptT e m) b -> OptimizedAutomatonT m (Either e b)
exceptS automaton = Stateful $ AutomatonT.exceptS $ toAutomatonT automaton

applyExcept :: (Monad m) => OptimizedAutomatonT (ExceptT (e1 -> e2) m) a -> OptimizedAutomatonT (ExceptT e1 m) a -> OptimizedAutomatonT (ExceptT e2 m) a
applyExcept automatonF automatonA = Stateful $ AutomatonT.applyExcept (toAutomatonT automatonF) (toAutomatonT automatonA)
