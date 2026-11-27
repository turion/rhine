{- | Handle a global 'ListT' layer in an 'Automaton'.

'Automaton's in the 'ListT' transformer can produce multiple outputs for
each input, or none. This enables dynamic spawning and stopping of automata.
-}
module Data.Automaton.Trans.List (
  module List.Transformer,
  widthFirst,
  sequenceS,
  fromFoldable,
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
{-# INLINE widthFirst #-}

-- | Build an 'Automaton' in the 'ListT' transformer by broadcasting the input to each automaton in a given list.
sequenceS :: (Monad m) => [Automaton m a b] -> Automaton (ListT m) a b
sequenceS = asum . fmap liftS
{-# INLINE sequenceS #-}

-- | Construct from an arbitrary Foldable (a wrapper around 'select').
fromFoldable :: (Monad m, Foldable f) => f a -> ListT m a
fromFoldable = select
{-# INLINE fromFoldable #-}

-- | Fold into a list.
toList :: (Monad m) => ListT m a -> m [a]
toList = fold (flip (:)) [] reverse
{-# INLINE toList #-}
