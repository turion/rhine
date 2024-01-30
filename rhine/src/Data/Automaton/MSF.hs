{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Automaton.MSF where

import Control.Arrow
import Control.Category
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Automaton
import Data.Automaton qualified as Automaton
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Profunctor (Choice (..), Profunctor (..), Strong)
import Data.Profunctor.Strong (Strong (..))
import Data.Profunctor.Traversing
import Data.VectorSpace (VectorSpace (..))
import Prelude hiding (id, (.))

newtype MSF m a b = MSF {getMSF :: AutomatonT (ReaderT a m) b}
  deriving newtype (Functor, Applicative, Num, Fractional, Floating)

instance (Eq s, Floating s, VectorSpace v s, Applicative m) => VectorSpace (MSF m a v) (MSF m a s) where
  zeroVector = MSF zeroVector
  MSF s *^ MSF v = coerce $ s *^ v
  MSF v1 ^+^ MSF v2 = coerce $ v1 ^+^ v2
  dot (MSF s) (MSF v) = coerce $ dot s v
  normalize (MSF v) = coerce v

instance (Monad m) => Category (MSF m) where
  id = MSF $! automatonT () $! const $! asks $! Result ()
  {-# INLINE id #-}

  MSF (AutomatonT stateF0 stepF) . MSF (AutomatonT stateG0 stepG) =
    MSF
      $! automatonT
        (JointState stateF0 stateG0)
        ( \(JointState stateF stateG) -> do
            Result stateG' b <- stepG stateG
            Result stateF' c <- lift $! runReaderT (stepF stateF) b
            return $! Result (JointState stateF' stateG') c
        )
  {-# INLINE (.) #-}

instance (Monad m) => Arrow (MSF m) where
  arr f = MSF $! automatonT () $! const $! asks $! Result () . f
  {-# INLINE arr #-}

  first (MSF AutomatonT {state, step}) =
    MSF
      $! automatonT
        state
        ( \s ->
            ReaderT
              ( \(b, d) ->
                  fmap (,d)
                    <$> runReaderT (step s) b
              )
        )
  {-# INLINE first #-}

-- FIXME efficient parallelized ***?

instance (Monad m) => ArrowChoice (MSF m) where
  MSF (AutomatonT stateL0 stepL) +++ MSF (AutomatonT stateR0 stepR) =
    MSF
      $! automatonT
        (JointState stateL0 stateR0)
        ( \(JointState stateL stateR) ->
            ReaderT
              $! either
                (runReaderT (mapResultState (`JointState` stateR) . fmap Left <$> stepL stateL))
                (runReaderT (mapResultState (JointState stateL) . fmap Right <$> stepR stateR))
        )
  {-# INLINE (+++) #-}

arrM :: (Functor m) => (a -> m b) -> MSF m a b
arrM f = MSF $! Automaton.constM $! ReaderT f
{-# INLINE arrM #-}

constM :: (Functor m) => m b -> MSF m a b
constM = arrM . const
{-# INLINE constM #-}

hoistS :: (Functor n) => (forall x. m x -> n x) -> MSF m a b -> MSF n a b
hoistS morph (MSF AutomatonT {state, step}) = MSF $ automatonT state (mapReaderT morph . step)
{-# INLINE hoistS #-}

liftS :: (MonadTrans t, Monad m, Functor (t m)) => MSF m a b -> MSF (t m) a b
liftS = hoistS lift
{-# INLINE liftS #-}

feedback :: (Functor m) => c -> MSF m (a, c) (b, c) -> MSF m a b
feedback c (MSF AutomatonT {state, step}) =
  MSF
    $! automatonT
      (JointState state c)
      ( \(JointState s c) -> ReaderT $ \a -> (\(Result s (b, c)) -> Result (JointState s c) b) <$> runReaderT (step s) (a, c)
      )
{-# INLINE feedback #-}

count :: (Num n, Monad m) => MSF m a n
count = feedback 0 $! arr (\(_, n) -> let n' = n + 1 in (n', n'))
{-# INLINE count #-}

stepMSF :: (Functor m) => MSF m a b -> a -> m (StrictTuple b (MSF m a b))
stepMSF (MSF automatonT) a =
  runReaderT (stepAutomaton automatonT) a
    <&> (\(Result automaton b) -> StrictTuple b $! MSF automaton)
{-# INLINE stepMSF #-}

reactimate :: (Monad m) => MSF m () () -> m void
reactimate msf = do
  StrictTuple () msf' <- stepMSF msf ()
  reactimate msf'

-- FIXME rename to mapMSF?
morph :: (Functor m2) => (forall s. (a1 -> m1 (Result s b1)) -> (a2 -> m2 (Result s b2))) -> MSF m1 a1 b1 -> MSF m2 a2 b2
morph f = MSF . Automaton.morph (ReaderT . f . runReaderT) . getMSF
{-# INLINE morph #-}

-- FIXME maybe implement all methods?
instance (Monad m) => Profunctor (MSF m) where
  dimap f g MSF {getMSF} = MSF $ g <$> hoist (withReaderT f) getMSF

instance (Monad m) => Choice (MSF m) where
  left' = left

instance (Monad m) => Strong (MSF m) where
  first' = first

instance (Monad m) => Traversing (MSF m) where
  wander f MSF {getMSF = AutomatonT {state, step}} =
    MSF
      { getMSF =
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

mapMaybeS :: (Monad m) => MSF m a b -> MSF m (Maybe a) (Maybe b)
mapMaybeS = traverse'

-- FIXME not sure this is needed. Maybe instead use Result everywhere? Or regular tuple?
data StrictTuple a b = StrictTuple a b

newtype Final m a b = Final {getFinal :: a -> m (Result (Final m a b) b)}

toFinal :: (Functor m) => MSF m a b -> Final m a b
toFinal msf = Final $ fmap (\(StrictTuple b msf') -> Result (toFinal msf') b) <$> stepMSF msf

fromFinal :: Final m a b -> MSF m a b
fromFinal final =
  MSF
    AutomatonT
      { state = final
      , step = ReaderT . getFinal
      }

concatS :: (Monad m) => MSF m () [b] -> MSF m () b
concatS (MSF AutomatonT {state, step}) =
  MSF
    AutomatonT
      { state = (state, [])
      , step = go
      }
  where
    go (s, []) = do
      Result s' bs <- step s
      go (s', bs)
    go (s, b : bs) = return $! Result (s, bs) b

withSideEffect :: (Monad m) => (a -> m b) -> MSF m a a
withSideEffect f = (id &&& arrM f) >>> arr fst

accumulateWith :: (Monad m) => (a -> b -> b) -> b -> MSF m a b
accumulateWith f state =
  MSF
    AutomatonT
      { state
      , step = \b -> ReaderT $ \a -> let b' = f a b in return $! Result b' b'
      }
{-# INLINE accumulateWith #-}

delay :: (Applicative m) => a -> MSF m a a
delay a0 = MSF $ AutomatonT a0 $ \aState -> ReaderT $ \aIn -> pure $! Result aIn aState
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
embed _ [] = pure []
embed msf (a : as) = do
  StrictTuple b msf' <- stepMSF msf a
  bs <- embed msf' as
  return $ b : bs
