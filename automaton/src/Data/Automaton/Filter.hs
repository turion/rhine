{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}

module Data.Automaton.Filter where

-- base
import Control.Applicative (Alternative (..))
import Control.Arrow
import Control.Category (Category (..))
import Control.Monad (join)
import Data.Functor ((<&>))
import Prelude hiding (filter, id, (.))

-- profunctors
import Data.Profunctor.Traversing (Traversing (..))

-- witherable
import Witherable (Filterable (..), Witherable)

-- automaton
import Data.Automaton
import Data.Functor.Compose (Compose (..))

{- | An automaton that filters or traverses its output using a type operator @f@.

When @f@ is 'Maybe', then @'FilterAutomaton' 'Maybe' a b@ can filter in the sense that not every input necessarily leads to an output.

@f@ can also be a type that allows multiple positions, such as a list.
-}
newtype FilterAutomaton m f a b = FilterAutomaton
  { getFilterAutomaton :: Automaton m a (f b)
  -- ^ Interpret a 'FilterAutomaton'.
  --   For instance if @f = 'Maybe'@, the resulting automaton will output 'Nothing' whenever there is no output of the 'FilterAutomaton'.
  }
  deriving (Functor)
  deriving Applicative via Compose (Automaton m a) f
  deriving Alternative via Compose (Automaton m a) f

-- | Use a filtering function to create a 'FilterAutomaton'.
arrFilter :: (Monad m) => (a -> f b) -> FilterAutomaton m f a b
arrFilter = FilterAutomaton . arr

-- | Filter the input according to a predicate.
filterS :: (Monad m, Witherable f, Applicative f) => (a -> Bool) -> FilterAutomaton m f a a
filterS f = filter f id'

-- | Like 'Category.id', but only requiring @'Applicative' f@.
id' :: (Monad m, Applicative f) => FilterAutomaton m f a a
id' = FilterAutomaton $ arr pure

instance (Monad m, Traversable f, Monad f) => Category (FilterAutomaton m f) where
  id = id'
  FilterAutomaton g . FilterAutomaton f = FilterAutomaton $ (traverse' g . f) <&> join

instance (Monad m, Traversable f, Monad f) => Arrow (FilterAutomaton m f) where
  arr f = FilterAutomaton $ arr $ f >>> pure
  first (FilterAutomaton automaton) = FilterAutomaton $ first automaton <&> (\(fc, d) -> (,d) <$> fc)

instance (Traversable f, Monad m, Monad f) => ArrowChoice (FilterAutomaton m f) where
  FilterAutomaton automaton1 +++ FilterAutomaton automaton2 = FilterAutomaton $ automaton1 +++ automaton2 <&> either (fmap Left) (fmap Right)

instance (Functor m, Filterable f) => Filterable (FilterAutomaton m f a) where
  mapMaybe f (FilterAutomaton automaton) = FilterAutomaton $ automaton <&> mapMaybe f

-- | Lift a regular 'Automaton' (which doesn't filter) to a 'FilterAutomaton'.
liftFilter :: (Monad m, Applicative f) => Automaton m a b -> FilterAutomaton m f a b
liftFilter = FilterAutomaton . fmap pure

{- | Postcompose with an 'Automaton'.

The postcomposed automaton will be stepped for every output of the filter automaton.
-}
rmapS :: (Traversable f, Monad m) => FilterAutomaton m f a b -> Automaton m b c -> FilterAutomaton m f a c
rmapS (FilterAutomaton fa) a = FilterAutomaton $ fa >>> traverse' a

-- | Precompose with an 'Automaton'.
lmapS :: (Monad m) => Automaton m a b -> FilterAutomaton m f b c -> FilterAutomaton m f a c
lmapS a (FilterAutomaton fa) = FilterAutomaton $ a >>> fa
