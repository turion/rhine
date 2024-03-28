{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Automaton.MSF where

-- base
import Control.Arrow
import Control.Category
import Control.Monad ((<=<))
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Functor ((<&>))
import Prelude hiding (id, (.))

-- mmorph
import Control.Monad.Morph (MFunctor (..))

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

-- profunctors
import Data.Profunctor (Choice (..), Profunctor (..), Strong)
import Data.Profunctor.Strong (Strong (..))
import Data.Profunctor.Traversing
import Prelude hiding (id, (.))

-- simple-affine-space
import Data.VectorSpace (VectorSpace (..))

-- rhine

import Control.Applicative (Alternative)
import Data.Automaton (AutomatonT (..), JointState (..))
import Data.Automaton.Final qualified as AutomatonFinal
import Data.Automaton.Optimized (
  OptimizedAutomatonT (..),
  concatS,
  stepOptimizedAutomaton,
 )
import Data.Automaton.Optimized qualified as AutomatonOptimized
import Data.Automaton.Result

newtype MSF m a b = MSF {getMSF :: OptimizedAutomatonT (ReaderT a m) b}
  deriving newtype (Functor, Applicative, Alternative, Num, Fractional, Floating)

instance (Eq s, Floating s, VectorSpace v s, Applicative m) => VectorSpace (MSF m a v) (MSF m a s) where
  zeroVector = MSF zeroVector
  MSF s *^ MSF v = coerce $ s *^ v
  MSF v1 ^+^ MSF v2 = coerce $ v1 ^+^ v2
  dot (MSF s) (MSF v) = coerce $ dot s v
  normalize (MSF v) = coerce v

instance (Monad m) => Category (MSF m) where
  id = MSF $ Stateless ask
  {-# INLINE id #-}

  MSF (Stateful (AutomatonT stateF0 stepF)) . MSF (Stateful (AutomatonT stateG0 stepG)) =
    MSF $!
      Stateful $!
        AutomatonT
          { state = JointState stateF0 stateG0
          , step = \(JointState stateF stateG) -> do
              Result stateG' b <- stepG stateG
              Result stateF' c <- lift $! runReaderT (stepF stateF) b
              return $! Result (JointState stateF' stateG') c
          }
  MSF (Stateful (AutomatonT state0 step)) . MSF (Stateless m) =
    MSF $!
      Stateful $!
        AutomatonT
          { state = state0
          , step = \state -> do
              b <- m
              lift $! runReaderT (step state) b
          }
  MSF (Stateless m) . MSF (Stateful (AutomatonT state0 step)) =
    MSF $!
      Stateful $!
        AutomatonT
          { state = state0
          , step = \state -> do
              Result state' b <- step state
              c <- lift $! runReaderT m b
              return $! Result state' c
          }
  MSF (Stateless f) . MSF (Stateless g) = MSF $ Stateless $ ReaderT $ runReaderT f <=< runReaderT g
  {-# INLINE (.) #-}

instance (Monad m) => Arrow (MSF m) where
  arr f = MSF $! Stateless $! asks f
  {-# INLINE arr #-}

  first (MSF (Stateful AutomatonT {state, step})) =
    MSF $!
      Stateful $!
        AutomatonT
          { state
          , step = \s ->
              ReaderT
                ( \(b, d) ->
                    fmap (,d)
                      <$> runReaderT (step s) b
                )
          }
  first (MSF (Stateless m)) = MSF $ Stateless $ ReaderT $ \(b, d) -> (,d) <$> runReaderT m b
  {-# INLINE first #-}

instance (Monad m) => ArrowChoice (MSF m) where
  MSF (Stateful (AutomatonT stateL0 stepL)) +++ MSF (Stateful (AutomatonT stateR0 stepR)) =
    MSF $!
      Stateful $!
        AutomatonT
          { state = JointState stateL0 stateR0
          , step = \(JointState stateL stateR) ->
              ReaderT $!
                either
                  (runReaderT (mapResultState (`JointState` stateR) . fmap Left <$> stepL stateL))
                  (runReaderT (mapResultState (JointState stateL) . fmap Right <$> stepR stateR))
          }
  MSF (Stateless m) +++ MSF (Stateful (AutomatonT state0 step)) =
    MSF $!
      Stateful $!
        AutomatonT
          { state = state0
          , step = \state ->
              ReaderT $!
                either
                  (runReaderT . fmap (Result state . Left) $ m)
                  (runReaderT . fmap (fmap Right) $ step state)
          }
  MSF (Stateful (AutomatonT state0 step)) +++ MSF (Stateless m) =
    MSF $!
      Stateful $!
        AutomatonT
          { state = state0
          , step = \state ->
              ReaderT $!
                either
                  (runReaderT . fmap (fmap Left) $ step state)
                  (runReaderT . fmap (Result state . Right) $ m)
          }
  MSF (Stateless mL) +++ MSF (Stateless mR) =
    MSF $
      Stateless $
        ReaderT $
          either
            (runReaderT . fmap Left $ mL)
            (runReaderT . fmap Right $ mR)
  {-# INLINE (+++) #-}

  left (MSF (Stateful (AutomatonT {state, step}))) =
    MSF $!
      Stateful $!
        AutomatonT
          { state
          , step = \s -> ReaderT $ either (fmap (fmap Left) . runReaderT (step s)) (pure . Result s . Right)
          }
  left (MSF (Stateless ma)) = MSF $! Stateless $! ReaderT $! either (fmap Left . runReaderT ma) (pure . Right)
  {-# INLINE left #-}

  right (MSF (Stateful (AutomatonT {state, step}))) =
    MSF $!
      Stateful $!
        AutomatonT
          { state
          , step = \s -> ReaderT $ either (pure . Result s . Left) (fmap (fmap Right) . runReaderT (step s))
          }
  right (MSF (Stateless ma)) = MSF $! Stateless $! ReaderT $! either (pure . Left) (fmap Right . runReaderT ma)
  {-# INLINE right #-}

arrM :: (Functor m) => (a -> m b) -> MSF m a b
arrM f = MSF $! AutomatonOptimized.constM $! ReaderT f
{-# INLINE arrM #-}

constM :: (Functor m) => m b -> MSF m a b
constM = arrM . const
{-# INLINE constM #-}

hoistS :: (Monad m) => (forall x. m x -> n x) -> MSF m a b -> MSF n a b
hoistS morph (MSF automaton) = MSF $ hoist (mapReaderT morph) automaton
{-# INLINE hoistS #-}

liftS :: (MonadTrans t, Monad m, Functor (t m)) => MSF m a b -> MSF (t m) a b
liftS = hoistS lift
{-# INLINE liftS #-}

feedback :: (Functor m) => c -> MSF m (a, c) (b, c) -> MSF m a b
feedback c (MSF (Stateful AutomatonT {state, step})) =
  MSF $!
    Stateful $!
      AutomatonT
        { state = JointState state c
        , step = \(JointState s c) -> ReaderT $ \a -> (\(Result s (b, c)) -> Result (JointState s c) b) <$> runReaderT (step s) (a, c)
        }
feedback state (MSF (Stateless m)) =
  MSF $!
    Stateful $!
      AutomatonT
        { state
        , step = \c -> ReaderT $ \a -> (\(b, c) -> Result c b) <$> runReaderT m (a, c)
        }
{-# INLINE feedback #-}

count :: (Num n, Monad m) => MSF m a n
count = feedback 0 $! arr (\(_, n) -> let n' = n + 1 in (n', n'))
{-# INLINE count #-}

-- FIXME maybe I should remove this and instead one has to go via final
stepMSF :: (Functor m) => MSF m a b -> a -> m (StrictTuple b (MSF m a b))
stepMSF (MSF automatonT) a =
  runReaderT (stepOptimizedAutomaton automatonT) a
    <&> (\(Result automaton b) -> StrictTuple b $! MSF automaton)
{-# INLINE stepMSF #-}

reactimate :: (Monad m) => MSF m () () -> m void
reactimate (MSF automaton) = AutomatonOptimized.reactimate $ hoist (`runReaderT` ()) automaton

-- FIXME rename to mapMSF?
morph :: (Functor m1, Functor m2) => (forall s. (a1 -> m1 (Result s b1)) -> (a2 -> m2 (Result s b2))) -> MSF m1 a1 b1 -> MSF m2 a2 b2
morph f = MSF . AutomatonOptimized.mapOptimizedAutomatonT (ReaderT . f . runReaderT) . getMSF
{-# INLINE morph #-}

-- FIXME maybe implement all methods?
instance (Monad m) => Profunctor (MSF m) where
  dimap f g MSF {getMSF} = MSF $ g <$> hoist (withReaderT f) getMSF

instance (Monad m) => Choice (MSF m) where
  left' = left

instance (Monad m) => Strong (MSF m) where
  first' = first

instance (Monad m) => Traversing (MSF m) where
  wander f MSF {getMSF = Stateful AutomatonT {state, step}} =
    MSF
      { getMSF =
          Stateful
            AutomatonT
              { state
              , step =
                  step
                    & fmap runReaderT
                    & flip
                    & fmap ResultStateT
                    & f
                    & fmap getResultStateT
                    & flip
                    & fmap ReaderT
              }
      }
  wander f (MSF (Stateless m)) = MSF $ Stateless $ ReaderT $ f $ runReaderT $ m

mapMaybeS :: (Monad m) => MSF m a b -> MSF m (Maybe a) (Maybe b)
mapMaybeS = traverse'

-- FIXME docs that say that you absolutely need to use this! you can.t roll your own list pattern matching in arrow notation!
traverseS :: (Monad m, Traversable f) => MSF m a b -> MSF m (f a) (f b)
traverseS = traverse'

traverseS_ :: (Monad m, Traversable f) => MSF m a b -> MSF m (f a) ()
traverseS_ msf = traverse' msf >>> arr (const ())

-- FIXME naming isn't great?
mapS :: (Monad m) => (forall m. (Monad m) => AutomatonT m a -> AutomatonT m b) -> MSF m i a -> MSF m i b
mapS f = MSF . AutomatonOptimized.mapS f . getMSF

handleS :: (Monad m) => (AutomatonT (ReaderT a m) b -> AutomatonT (ReaderT c n) d) -> MSF m a b -> MSF n c d
handleS f = MSF . AutomatonOptimized.handleS f . getMSF

-- FIXME Right now it's not even strict because I didn't have StrictData
-- FIXME not sure this is needed. Maybe instead use Result everywhere? Or regular tuple?
data StrictTuple a b = StrictTuple a b

-- FIXME is this ever needed?
newtype Final m a b = Final {getFinal :: AutomatonFinal.Final (ReaderT a m) b}
  deriving (Functor, Applicative, Alternative)

instance (Monad m) => Category (Final m) where
  id = toFinal id
  f1 . f2 = toFinal $ fromFinal f1 . fromFinal f2

instance (Monad m) => Arrow (Final m) where
  arr = toFinal . arr
  first = toFinal . first . fromFinal

toFinal :: (Functor m) => MSF m a b -> Final m a b
toFinal (MSF automaton) = Final $ AutomatonOptimized.toFinal automaton

fromFinal :: Final m a b -> MSF m a b
fromFinal Final {getFinal} = MSF $ AutomatonOptimized.fromFinal getFinal

concatS :: (Monad m) => MSF m () [b] -> MSF m () b
concatS (MSF automaton) = MSF $ Data.Automaton.Optimized.concatS automaton

withSideEffect :: (Monad m) => (a -> m b) -> MSF m a a
withSideEffect f = (id &&& arrM f) >>> arr fst

accumulateWith :: (Monad m) => (a -> b -> b) -> b -> MSF m a b
accumulateWith f state =
  MSF $!
    Stateful $!
      AutomatonT
        { state
        , step = \b -> ReaderT $ \a -> let b' = f a b in return $! Result b' b'
        }
{-# INLINE accumulateWith #-}

delay :: (Applicative m) => a -> MSF m a a
delay a0 = MSF $ Stateful $! AutomatonT a0 $ \aState -> ReaderT $ \aIn -> pure $! Result aIn aState
{-# INLINE delay #-}

mappendFrom :: (Monoid w, Monad m) => w -> MSF m w w
mappendFrom = accumulateWith mappend

mappendS :: (Monoid w, Monad m) => MSF m w w
mappendS = mappendFrom mempty

sumFrom :: (VectorSpace v s, Monad m) => v -> MSF m v v
sumFrom = accumulateWith (^+^)

sumS :: (Monad m, VectorSpace v s) => MSF m v v
sumS = sumFrom zeroVector

embed :: (Monad m) => MSF m a b -> [a] -> m [b]
embed _msf [] = return []
embed msf (a : as) = do
  StrictTuple b msf' <- stepMSF msf a
  bs <- embed msf' as
  return $ b : bs
