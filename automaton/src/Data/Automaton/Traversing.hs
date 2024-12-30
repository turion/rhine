module Data.Automaton.Traversing where

-- base
import Control.Applicative (Alternative (..))
import Control.Arrow
import Control.Category (Category (..))
import Control.Monad (MonadPlus, join)
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose (..))
import Prelude hiding (id, (.))

-- profunctors
import Data.Profunctor (Profunctor (..))
import Data.Profunctor.Traversing (Traversing (..))

-- automaton
import Data.Automaton

-- FIXME some basic unit tests

{- | An 'Automaton' with a 'Traversable' output shape @f@.

When two such traversing automata are composed, the second one automatically traverses all the output of the first one, and joins it together.

A typical application is filtering a stream.
For this, see the specialisation 'FilterAutomaton'.

For some example types of @f@, a composition @ta1 >>> ta2@ has the following behaviour:

* Lists: For every list element in the output of @ta1@, one step of @ta2@ is performed, and all results are concatenated.
  Useful for exploration algorithms.
* 'NonEmpty': Like lists.
* 'Maybe': @ta2@ is only stepped when @ta1@ produces 'Just' (and the composition is only 'Just' when @ta2@ also produces a 'Just'). See 'FilterAutomaton' for details.
* 'Either': Like 'Maybe', but also produce the 'Left' value of the earliest automaton.

@f@ usually has to be an instance of both 'Traversable' and 'Monad' for this type to be useful.
-}
newtype TraversingAutomaton m f a b = TraversingAutomaton {getTraversingAutomaton :: Automaton m a (f b)}
  deriving (Functor)
  deriving (Applicative) via (Compose (Automaton m a) f)
  deriving (Alternative) via (Compose (Automaton m a) f)

instance (Functor m, Functor f) => Profunctor (TraversingAutomaton m f) where
  dimap f g (TraversingAutomaton automaton) = TraversingAutomaton $ dimap f (fmap g) automaton

instance (Monad m, Traversable f, Monad f) => Category (TraversingAutomaton m f) where
  id = TraversingAutomaton $ arr return
  TraversingAutomaton g . TraversingAutomaton f = TraversingAutomaton $ join <$> traverse' g . f

instance (Monad m, Traversable f, Monad f) => Arrow (TraversingAutomaton m f) where
  arr f = TraversingAutomaton $ arr $ f >>> pure
  first (TraversingAutomaton automaton) = TraversingAutomaton $ first automaton >>> arr (\(fc, d) -> (,d) <$> fc)

instance (Traversable f, Monad m, Monad f) => ArrowChoice (TraversingAutomaton m f) where
  TraversingAutomaton automaton1 +++ TraversingAutomaton automaton2 = TraversingAutomaton $ automaton1 +++ automaton2 <&> either (fmap Left) (fmap Right)

instance (Traversable f, Monad m, MonadPlus m, Monad f) => ArrowZero (TraversingAutomaton m f) where
  zeroArrow = empty

instance (Traversable f, Monad m, MonadPlus m, Monad f) => ArrowPlus (TraversingAutomaton m f) where
  (<+>) = (<|>)

-- | Lift a pure function with output shape @f@.
arrT :: (Applicative m) => (a -> f b) -> TraversingAutomaton m f a b
arrT = TraversingAutomaton . arr'

-- | Lift an automaton that always returns a single value.
liftTraversing :: (Applicative m, Applicative f) => Automaton m a b -> TraversingAutomaton m f a b
liftTraversing = TraversingAutomaton . fmap pure

-- | Compose on the left with an automaton that always returns a single value.
lmapS :: (Traversable f, Monad m) => Automaton m a b -> TraversingAutomaton m f b c -> TraversingAutomaton m f a c
lmapS ab (TraversingAutomaton bc) = TraversingAutomaton $ ab >>> bc

-- | Compose on the right with an automaton that always returns a single value.
rmapS :: (Traversable f, Monad m) => TraversingAutomaton m f a b -> Automaton m b c -> TraversingAutomaton m f a c
rmapS (TraversingAutomaton ab) bc = TraversingAutomaton $ ab >>> traverseS bc

{- | Compose on the left with a pure function with output shape @f@.

Note: In contrast to 'rmapT', the automaton has to traverse all values of @f b@, requiring the @'Monad' m@ instance.
-}
lmapT :: (Monad f, Monad m, Traversable f) => (a -> f b) -> TraversingAutomaton m f b c -> TraversingAutomaton m f a c
lmapT f (TraversingAutomaton automaton) = TraversingAutomaton $ dimap f join $ traverseS automaton

-- | Compose on the right with a pure function with output shape @f@.
rmapT :: (Functor m, Monad f) => (b -> f c) -> TraversingAutomaton m f a b -> TraversingAutomaton m f a c
rmapT f (TraversingAutomaton automaton) = TraversingAutomaton $ (>>= f) <$> automaton
