{- | This module combines the wrapping and running functions for the 'Reader',
'Writer' and 'State' monad layers in a single layer.

It is based on the /strict/ 'RWS' monad 'Control.Monad.Trans.RWS.Strict',
so when combining it with other modules such as @mtl@'s, the strict version
has to be included, i.e. 'Control.Monad.RWS.Strict' instead of
'Control.Monad.RWS' or 'Control.Monad.RWS.Lazy'.
-}
module Data.Automaton.Trans.RWS (
  module Data.Automaton.Trans.RWS,
  module Control.Monad.Trans.RWS.Strict,
)
where

-- transformers
import Control.Monad.Trans.RWS.Strict hiding (liftCallCC, liftCatch)

-- automaton
import Data.Automaton (Automaton, withAutomaton)
import Data.Stream.Result (Result (..))

-- * 'RWS' (Reader-Writer-State) monad

-- | Wrap an 'Automaton' with explicit state variables in 'RWST' monad transformer.
rwsS ::
  (Functor m, Monad m, Monoid w) =>
  Automaton m (r, s, a) (w, s, b) ->
  Automaton (RWST r w s m) a b
rwsS = withAutomaton $ \f a -> RWST $ \r s ->
  (\(Result c (w, s', b)) -> (Result c b, s', w))
    <$> f (r, s, a)

-- | Run the 'RWST' layer by making the state variables explicit.
runRWSS ::
  (Functor m, Monad m, Monoid w) =>
  Automaton (RWST r w s m) a b ->
  Automaton m (r, s, a) (w, s, b)
runRWSS = withAutomaton $ \f (r, s, a) ->
  (\(Result c b, s', w) -> Result c (w, s', b))
    <$> runRWST (f a) r s
