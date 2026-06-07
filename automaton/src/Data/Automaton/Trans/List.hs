{- | Handle a global 'ListT' layer in an 'Automaton'.

'Automaton's in the 'ListT' transformer can produce multiple outputs for
each input, or none. This enables dynamic spawning and stopping of automata.
-}
module Data.Automaton.Trans.List (
  module List.Transformer,
  widthFirst,
  sequenceS,
  fromList,
  toList,
)
where

-- base
import Control.Applicative (asum)

-- list-transformer
import List.Transformer

-- automaton
import Data.Automaton (Automaton, handleListT, liftS)

{- | Run an 'Automaton' in the 'ListT' transformer by applying the input to
each automaton in the list transformer and concatenating the outputs.
-}
widthFirst :: (Monad m) => Automaton (ListT m) a b -> Automaton m a [b]
widthFirst = handleListT

-- | Build an 'Automaton' in the 'ListT' transformer by broadcasting the input to each automaton in a given list.
sequenceS :: (Monad m) => [Automaton m a b] -> Automaton (ListT m) a b
sequenceS = asum . fmap liftS

-- | Construct from a list.
fromList :: (Monad m) => [a] -> ListT m a
fromList = foldr cons empty

-- | Fold into a list.
toList :: (Monad m) => ListT m a -> m [a]
toList = fmap reverse . fold (flip (:)) [] id

cons :: (Monad m) => a -> ListT m a -> ListT m a
cons x xs = ListT $ pure $ Cons x xs
