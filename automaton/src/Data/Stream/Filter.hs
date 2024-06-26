{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}

module Data.Stream.Filter where

-- base
import Control.Applicative (Alternative (..))
import Control.Category (Category (..))
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose (..))
import Prelude hiding (filter, id, (.))

-- witherable
import Witherable (Filterable (..), Witherable (..))

-- semialign
import Data.Align (Align (..), Semialign (..))

-- automaton
import Data.Stream (StreamT, constM)

{- | A stream that filters or traverses its output using a type operator @f@.

When @f@ is 'Maybe', then @'FilterStream' 'Maybe' a@ can filter in the sense that on a given step there might not be an output.

@f@ can also be a type that allows multiple positions, such as lists, or 'NonEmpty'.
In this case, 'FilterStream' branches out and can explore multiple outputs at the same time.
(It keeps a single state, though.)
-}
newtype FilterStream m f a = FilterStream
  { getFilterStream :: StreamT m (f a)
  -- ^ Interpret a 'FilterStream'.
  --   For instance if @f = 'Maybe'@, the resulting stream will output 'Nothing' whenever there is no output of the 'FilterStream'.
  }
  deriving (Functor, Foldable, Traversable)
  deriving (Applicative) via Compose (StreamT m) f
  deriving (Alternative) via Compose (StreamT m) f

-- | Use a filtered value to create a 'FilterStream'.
constFilter :: (Applicative m) => f a -> FilterStream m f a
constFilter = FilterStream . pure

-- | Use an effectful filtered value to create a 'FilterStream'.
filterM :: (Functor m) => m (f a) -> FilterStream m f a
filterM = FilterStream . constM

-- | Filter a stream according to a predicate.
filterS :: (Monad m, Witherable f, Applicative f) => (a -> Bool) -> FilterStream m f a -> FilterStream m f a
filterS f = FilterStream . fmap (filter f) . getFilterStream

-- | Lift a regular 'StreamT' (which doesn't filter) to a 'FilterStream'.
liftFilter :: (Monad m, Applicative f) => StreamT m a -> FilterStream m f a
liftFilter = FilterStream . fmap pure

instance (Functor m, Filterable f) => Filterable (FilterStream m f) where
  mapMaybe f (FilterStream automaton) = FilterStream $ automaton <&> mapMaybe f

instance (Functor m, Traversable m, Filterable f, Traversable f) => Witherable (FilterStream m f)

-- | Run two streams in parallel and 'align' their outputs.
instance (Semialign f, Applicative m) => Semialign (FilterStream m f) where
  align a b = FilterStream $ align <$> getFilterStream a <*> getFilterStream b

instance (Align f, Applicative m) => Align (FilterStream m f) where
  nil = constFilter nil
