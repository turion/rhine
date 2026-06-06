{- | Handle a global 'ListT' layer in an 'Automaton'.

'Automaton's in the 'ListT' transformer can produce multiple outputs for
each input, or none. This enables dynamic spawning and stopping of automata.
-}
module Data.Automaton.Trans.List (
  module ListT,
  widthFirst,
  sequenceS,
  mapAutomaton,
)
where

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (runReaderT)

-- list-t
import ListT hiding (traverse, unfoldM)

-- automaton
import Data.Automaton (Automaton (..), getAutomaton, traverseS, unfoldM)
import Data.Stream.Optimized (stepOptimizedStream)
import Data.Stream.Result (Result (..))

-- base
import Data.List (singleton)

{- | Run an 'Automaton' in the 'ListT' transformer by applying the input to
each automaton in the list transformer and concatenating the outputs.
-}
widthFirst :: (Functor m, Monad m) => Automaton (ListT m) a b -> Automaton m a [b]
widthFirst = flip unfoldM step . singleton
  where
    step a as = do
      results <- concat <$> traverse (stepOne a) as
      let (bs, as') = unzip results
      pure $ Result as' bs
    stepOne a auto =
      toList $
        (\(Result auto' b) -> (b, Automaton auto'))
          <$> runReaderT (stepOptimizedStream (getAutomaton auto)) a

-- | Build an 'Automaton' in the 'ListT' transformer by broadcasting the input to each automaton in a given list.
sequenceS :: (Monad m) => [Automaton m a b] -> Automaton (ListT m) a b
sequenceS = flip unfoldM step
  where
    step a auto = do
      results <- lift $ traverse (stepOne a) auto
      fromFoldable results
    stepOne a auto = do
      Result auto' b <- runReaderT (stepOptimizedStream (getAutomaton auto)) a
      pure $ Result [Automaton auto'] b

-- | Apply an 'Automaton' to every input.
mapAutomaton :: (Monad m) => Automaton m a b -> Automaton m [a] [b]
mapAutomaton = traverseS
