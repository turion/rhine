{-# LANGUAGE DerivingVia #-}

{- | Often, we want to work with automata that don't produce exactly one output per input.
In these situations, 'FilterAutomaton' is a good abstraction.

For example, we might want to filter out certain values at some point, and only work with the remaining ones from there on.
So for each input, the automaton will either output a value, or it will not output a value.
A simple ad hoc solution is to create an automaton of type @'Automaton' m a ('Maybe' b)@,
where 'Nothing' encodes the absence of a value.
When composing a longer chain of such filtering automata, it often gets tedious to join all of the 'Maybe' layers.
'FilterAutomaton' abstracts this.

Instead of locking into the concrete 'Maybe' type, a more general API based on well-established type classes such as 'Witherable' was chosen.
As a result, it can be used with a custom type encoding optionality,
and even generalised to situations where you might have several outputs per input, using e.g. the list monad.
-}
module Data.Automaton.Filter where

-- base
import Control.Applicative (Alternative (..))
import Control.Arrow
import Control.Category (Category (..))
import Control.Monad (join)
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose (..))
import Prelude hiding (filter, id, (.))

-- transformers
import Control.Monad.Trans.Reader (ReaderT (..))

-- profunctors
import Data.Profunctor (Choice (..), Profunctor (..), Strong (..))
import Data.Profunctor.Traversing (Traversing (..))

-- witherable
import Witherable (Filterable (..))

-- semialign
import Data.Align (Align (..), Semialign)
import Data.Semialign (Semialign (..))

-- automaton
import Data.Automaton
import Data.Stream (hoist')
import Data.Stream.Filter (FilterStream (..), streamFilter)
import Data.Stream.Optimized (OptimizedStreamT (Stateful))

{- | An automaton that filters or traverses its output using a type operator @f@.

When @f@ is 'Maybe', then @'FilterAutomaton' 'Maybe' a b@ can filter in the sense that not every input necessarily leads to an output.

@f@ can also be a type that allows multiple positions, such as lists, or 'NonEmpty'.
In this case, 'FilterAutomaton' branches out and can explore multiple outputs at the same time.
(It keeps a single state, though.)
-}
newtype FilterAutomaton m f a b = FilterAutomaton
  { getFilterAutomaton :: Automaton m a (f b)
  -- ^ Interpret a 'FilterAutomaton'.
  --   For instance if @f = 'Maybe'@, the resulting automaton will output 'Nothing' whenever there is no output of the 'FilterAutomaton'.
  }
  deriving (Functor, Applicative, Alternative) via Compose (Automaton m a) f

-- | Create a 'FilterAutomaton' that ignores its input from a 'FilterStream'.
fromFilterStream :: (Monad m) => FilterStream m f a -> FilterAutomaton m f any a
fromFilterStream = FilterAutomaton . fromStream . getFilterStream

{- | Create a 'FilterStream' from a 'FilterAutomaton'.

The current input can be read as an effect in 'ReaderT'.
-}
toFilterStream :: (Functor m) => FilterAutomaton m f a b -> FilterStream (ReaderT a m) f b
toFilterStream = FilterStream . toStreamT . getFilterAutomaton

-- | Use a filtering function to create a 'FilterAutomaton'.
arrFilter :: (Monad m) => (a -> f b) -> FilterAutomaton m f a b
arrFilter = FilterAutomaton . arr

-- | Filter the input according to a predicate.
filterS :: (Monad m, Filterable f, Applicative f) => (a -> Bool) -> FilterAutomaton m f a a
filterS f = filter f id'

{- | Given an @f@-effect in the step, push it into the output type.

This works by internally tracking the @f@ effects in the state, and at the same time joining them in the output.

For example, if @f@ is lists, and @automaton :: Automaton (Compose m []) a b@ creates a 2-element list at some point,
the internal state of @automatonFilter automaton@ will split into two, and there are two outputs.

Likewise, if @f@ is 'Maybe', and a 'Nothing' occurs at some point, then this automaton is deactivated forever.
-}
automatonFilter :: (Monad f, Traversable f, Monad m) => Automaton (Compose m f) a b -> FilterAutomaton m f a b
automatonFilter = FilterAutomaton . Automaton . Stateful . getFilterStream . streamFilter . hoist' (\ramf -> Compose $ ReaderT $ getCompose . runReaderT ramf) . toStreamT

-- | Like 'Category.id', but only requiring @'Applicative' f@.
id' :: (Monad m, Applicative f) => FilterAutomaton m f a a
id' = arrFilter pure

instance (Monad m, Traversable f, Monad f) => Category (FilterAutomaton m f) where
  id = id'
  FilterAutomaton g . FilterAutomaton f = FilterAutomaton $ traverse' g . f <&> join

instance (Monad m, Traversable f, Monad f) => Arrow (FilterAutomaton m f) where
  arr f = FilterAutomaton $ arr $ f >>> pure
  first (FilterAutomaton automaton) = FilterAutomaton $ first automaton <&> (\(fc, d) -> (,d) <$> fc)

instance (Traversable f, Monad m, Monad f) => ArrowChoice (FilterAutomaton m f) where
  FilterAutomaton automaton1 +++ FilterAutomaton automaton2 = FilterAutomaton $ automaton1 +++ automaton2 <&> either (fmap Left) (fmap Right)

-- | There is no 'Witherable' instance though since it isn't 'Traversable'.
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

instance (Functor m, Functor f) => Profunctor (FilterAutomaton m f) where
  dimap f g FilterAutomaton {getFilterAutomaton} = FilterAutomaton $ dimap f (fmap g) getFilterAutomaton

instance (Monad m, Monad f, Traversable f) => Strong (FilterAutomaton m f) where
  first' = first

instance (Monad m, Monad f, Traversable f) => Choice (FilterAutomaton m f) where
  left' = left

-- | Run two automata in parallel and 'align' their outputs.
instance (Applicative m, Semialign f) => Semialign (FilterAutomaton m f a) where
  align fa1 fa2 = FilterAutomaton $ align <$> getFilterAutomaton fa1 <*> getFilterAutomaton fa2

-- | Constantly output the empty shape.
instance (Applicative m, Align f) => Align (FilterAutomaton m f a) where
  nil = FilterAutomaton $ constM $ pure nil
