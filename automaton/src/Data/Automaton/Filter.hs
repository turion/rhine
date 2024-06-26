{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
module Data.Automaton.Filter where

import Control.Monad (join)
import Data.Automaton
import Control.Category (Category (..))
import Prelude hiding (id, (.))
import Control.Arrow
import Data.Profunctor.Traversing (Traversing(..))
import Witherable (Filterable (..))
import Control.Applicative (Alternative (..))
import Data.Functor ((<&>))

-- FIXME some basic unit tests

newtype FilterAutomaton m f a b = FilterAutomaton {getFilterAutomaton :: Automaton m a (f b)}
  deriving (Functor)

instance (Monad m, Traversable f, Monad f) => Category (FilterAutomaton m f) where
  id = FilterAutomaton $ arr return
  FilterAutomaton g . FilterAutomaton f = FilterAutomaton $ arr join . traverse' g . f

instance (Monad m, Traversable f, Monad f) => Arrow (FilterAutomaton m f) where
  arr f = FilterAutomaton $ arr $ f >>> pure
  first (FilterAutomaton automaton) = FilterAutomaton $ first automaton >>> arr (\(fc, d) -> (, d) <$> fc)

instance (Monad m, Traversable f, Monad f) => Applicative (FilterAutomaton m f a) where
  pure = arr . const
  automaton1 <*> automaton2 = automaton1 &&& automaton2 >>> arr (uncurry id)

-- | Combine the output
instance (Monad m, Traversable f, Monad f, Alternative f) => Alternative (FilterAutomaton m f a) where
  empty = FilterAutomaton $ pure empty
  FilterAutomaton automaton1 <|> FilterAutomaton automaton2 = FilterAutomaton $ automaton1 &&& automaton2 >>> arr (uncurry (<|>))

instance (Traversable f, Monad m, Monad f) => ArrowChoice (FilterAutomaton m f) where
  FilterAutomaton automaton1 +++ FilterAutomaton automaton2 = FilterAutomaton $ automaton1 +++ automaton2 <&> either (fmap Left) (fmap Right)

-- FIXME rename
liftAsync :: (Monad m, Applicative f) => Automaton m a b -> FilterAutomaton m f a b
liftAsync = FilterAutomaton . (fmap pure)

rmapS :: (Traversable f, Monad m, Monad f) => FilterAutomaton m f a b -> Automaton m b c -> FilterAutomaton m f a c
-- FIXME version w/o Monad f exists
rmapS aa a = aa >>> liftAsync a

-- FIXME In principle could get away with Functor m, using Profunctor Automaton
instance Monad m => Filterable (FilterAutomaton m Maybe a) where
  mapMaybe f automaton = automaton >>> FilterAutomaton (arr f)
