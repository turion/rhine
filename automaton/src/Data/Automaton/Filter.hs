module Data.Automaton.Filter where

-- base
import Control.Monad (guard)
import Prelude hiding (id, (.))

-- witherable
import Witherable (Filterable (..))

-- automaton

import Data.Automaton
import Data.Automaton.Traversing

-- * 'FilterAutomaton'

{- | An automaton that can not only process, but also filter data.

When several filter automata are composed, only that data is output which passes through all filters.

For example:
@
evens = runFilterAutomaton $ liftFilter count >>> filterS even
@
This automaton will perform a step for every number, but output @Nothing, Just 2, Nothing, Just 4, ...@.

To arrive at a stream that does not output the 'Nothing' values, see 'Data.Automaton.catMaybeS'.
-}
type FilterAutomaton m = TraversingAutomaton m Maybe

instance (Functor m) => Filterable (FilterAutomaton m a) where
  mapMaybe = rmapT

liftFilter :: (Applicative m) => Automaton m a b -> FilterAutomaton m a b
liftFilter = liftTraversing

-- | In general, create a 'FilterAutomaton' from an automaton that only optionally outputs values.
filterAutomaton :: Automaton m a (Maybe b) -> FilterAutomaton m a b
filterAutomaton = TraversingAutomaton

-- | Once all filters are composed, retrieve the underlying automaton.
runFilterAutomaton :: FilterAutomaton m a b -> Automaton m a (Maybe b)
runFilterAutomaton = getTraversingAutomaton

filterS :: (Applicative m) => (a -> Bool) -> FilterAutomaton m a a
filterS f = arrFilter $ \a -> guard (f a) >> pure a

arrFilter :: (Applicative m) => (a -> Maybe b) -> FilterAutomaton m a b
arrFilter = arrT
