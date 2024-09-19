{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}

module Data.Stream.Filter where

-- base
import Control.Applicative (Alternative (..))
import Control.Category (Category (..))
import Control.Monad (forM, join)
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose (..))
import Prelude hiding (filter, id, (.))

-- witherable
import Witherable (Filterable (..), Witherable (..))

-- semialign
import Data.Align (Align (..), Semialign (..))

-- automaton
import Data.Stream (StreamT (..), constM)
import Data.Stream.Result (unzipResult)

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

{- | Given an @f@-effect in the step function of a stream, push it into the output type.

This works by internally tracking the @f@ effects in the state, and at the same time joining them in the output.

For example, if @f@ is lists, and @stream :: StreamT (Compose m []) a@ creates a 2-element list at some point,
the internal state of @streamFilter stream@ will split into two, and there are two outputs.

Likewise, if @f@ is 'Maybe', and a 'Nothing' occurs at some point, then this automaton is deactivated forever.
-}
streamFilter :: (Monad f, Traversable f, Monad m) => StreamT (Compose m f) a -> FilterStream m f a
streamFilter StreamT {state, step} =
  FilterStream $
    StreamT
      { state = pure state
      , step = \states -> unzipResult . join <$> forM states (getCompose . step)
      }

-- | Given a branching stream, concatenate all branches at every step.
runListS :: (Monad m) => StreamT (Compose m []) a -> StreamT m [a]
runListS = getFilterStream . streamFilter

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
