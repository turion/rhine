{- | Handle a global 'ListT' layer in an 'Automaton'.

'Automaton's in the 'ListT' transformer can produce multiple outputs for
each input, or none. This enables dynamic spawning and stopping of automata.
-}
module Data.Automaton.Trans.List (
  module ListT,
  widthFirst,
  sequenceS,
)
where

-- base
import Control.Applicative (asum)

-- list-t
import ListT

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
