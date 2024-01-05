{- | An 'Automaton's in a monad supporting random number generation (i.e.
having the 'RandT' layer in its stack) can be run.

Running means supplying an initial random number generator,
where the update of the generator at every random number generation is already taken care of.

Under the hood, 'RandT' is basically just 'StateT', with the current random
number generator as mutable state.
-}
module Data.Automaton.Trans.Random (
  runRandS,
  evalRandS,
  getRandomS,
  getRandomsS,
  getRandomRS,
  getRandomRS_,
  getRandomsRS,
  getRandomsRS_,
)
where

-- base
import Control.Arrow (arr, (>>>))

-- MonadRandom
import Control.Monad.Random (
  MonadRandom,
  RandT,
  Random,
  RandomGen,
  getRandom,
  getRandomR,
  getRandomRs,
  getRandoms,
  runRandT,
 )

-- automaton
import Data.Automaton (Automaton, arrM, constM, hoistS)
import Data.Automaton.Trans.State (StateT (..), runStateS_)

-- Creating random values

-- | Create a stream of random values.
getRandomS :: (MonadRandom m, Random b) => Automaton m a b
getRandomS = constM getRandom

-- | Create a stream of lists of random values.
getRandomsS :: (MonadRandom m, Random b) => Automaton m a [b]
getRandomsS = constM getRandoms

-- | Create a stream of random values in a given fixed range.
getRandomRS :: (MonadRandom m, Random b) => (b, b) -> Automaton m a b
getRandomRS range = constM $ getRandomR range

{- | Create a stream of random values in a given range, where the range is
specified on every tick.
-}
getRandomRS_ :: (MonadRandom m, Random b) => Automaton m (b, b) b
getRandomRS_ = arrM getRandomR

-- | Create a stream of lists of random values in a given fixed range.
getRandomsRS :: (MonadRandom m, Random b) => (b, b) -> Automaton m a [b]
getRandomsRS range = constM $ getRandomRs range

{- | Create a stream of lists of random values in a given range, where the
range is specified on every tick.
-}
getRandomsRS_ :: (MonadRandom m, Random b) => Automaton m (b, b) [b]
getRandomsRS_ = arrM getRandomRs

-- * Running automata with random effects

{- | Run an 'Automaton' in the 'RandT' random number monad transformer by supplying
an initial random generator. Updates and outputs the generator every step.
-}
runRandS ::
  (RandomGen g, Functor m, Monad m) =>
  Automaton (RandT g m) a b ->
  -- | The initial random number generator.
  g ->
  Automaton m a (g, b)
runRandS = runStateS_ . hoistS (StateT . runRandT)

{- | Evaluate an 'Automaton' in the 'RandT' transformer, i.e. extract possibly random
values by supplying an initial random generator. Updates the generator every
step but discards the generator.
-}
evalRandS ::
  (RandomGen g, Functor m, Monad m) =>
  Automaton (RandT g m) a b ->
  g ->
  Automaton m a b
evalRandS automaton g = runRandS automaton g >>> arr snd
