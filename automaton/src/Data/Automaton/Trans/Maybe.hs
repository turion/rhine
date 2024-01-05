-- | An 'Automaton' with 'Maybe' or 'MaybeT' in its monad stack can terminate execution at any step.
module Data.Automaton.Trans.Maybe (
  module Data.Automaton.Trans.Maybe,
  module Control.Monad.Trans.Maybe,
  maybeToExceptS,
)
where

-- base
import Control.Arrow (arr, returnA, (>>>))

-- transformers
import Control.Monad.Trans.Maybe hiding (
  liftCallCC,
  liftCatch,
  liftListen,
  liftPass,
 )

-- automaton
import Data.Automaton (Automaton, arrM, constM, hoistS, liftS)
import Data.Automaton.Trans.Except (
  ExceptT,
  exceptS,
  listToAutomatonExcept,
  maybeToExceptS,
  reactimateExcept,
  runAutomatonExcept,
  runExceptT,
  safe,
  safely,
  try,
 )

-- * Throwing 'Nothing' as an exception ("exiting")

-- | Throw the exception immediately.
exit :: (Monad m) => Automaton (MaybeT m) a b
exit = constM $ MaybeT $ return Nothing

-- | Throw the exception when the condition becomes true on the input.
exitWhen :: (Monad m) => (a -> Bool) -> Automaton (MaybeT m) a a
exitWhen condition = proc a -> do
  _ <- exitIf -< condition a
  returnA -< a

-- | Exit when the incoming value is 'True'.
exitIf :: (Monad m) => Automaton (MaybeT m) Bool ()
exitIf = proc condition ->
  if condition
    then exit -< ()
    else returnA -< ()

-- | @Just a@ is passed along, 'Nothing' causes the whole 'Automaton' to exit.
maybeExit :: (Monad m) => Automaton (MaybeT m) (Maybe a) a
maybeExit = inMaybeT

-- | Embed a 'Maybe' value in the 'MaybeT' layer. Identical to 'maybeExit'.
inMaybeT :: (Monad m) => Automaton (MaybeT m) (Maybe a) a
inMaybeT = arrM $ MaybeT . return

-- * Catching Maybe exceptions

-- | Run the first automaton until the second one produces 'True' from the output of the first.
untilMaybe :: (Monad m) => Automaton m a b -> Automaton m b Bool -> Automaton (MaybeT m) a b
untilMaybe automaton cond = proc a -> do
  b <- liftS automaton -< a
  c <- liftS cond -< b
  inMaybeT -< if c then Nothing else Just b

{- | When an exception occurs in the first 'automaton', the second 'automaton' is executed
from there.
-}
catchMaybe ::
  (Functor m, Monad m) =>
  Automaton (MaybeT m) a b ->
  Automaton m a b ->
  Automaton m a b
catchMaybe automaton1 automaton2 = safely $ try (maybeToExceptS automaton1) >> safe automaton2

-- * Converting to and from 'MaybeT'

-- | Convert exceptions into `Nothing`, discarding the exception value.
exceptToMaybeS ::
  (Functor m, Monad m) =>
  Automaton (ExceptT e m) a b ->
  Automaton (MaybeT m) a b
exceptToMaybeS =
  hoistS $ MaybeT . fmap (either (const Nothing) Just) . runExceptT

{- | Converts a list to an 'Automaton' in 'MaybeT', which outputs an element of the
list at each step, throwing 'Nothing' when the list ends.
-}
listToMaybeS :: (Functor m, Monad m) => [b] -> Automaton (MaybeT m) a b
listToMaybeS = exceptToMaybeS . runAutomatonExcept . listToAutomatonExcept

-- * Running 'MaybeT'

{- | Remove the 'MaybeT' layer by outputting 'Nothing' when the exception occurs.

The current state is then tested again on the next input.
-}
runMaybeS :: (Functor m, Monad m) => Automaton (MaybeT m) a b -> Automaton m a (Maybe b)
runMaybeS automaton = exceptS (maybeToExceptS automaton) >>> arr eitherToMaybe
  where
    eitherToMaybe (Left ()) = Nothing
    eitherToMaybe (Right b) = Just b

-- | 'reactimate's an 'Automaton' in the 'MaybeT' monad until it throws 'Nothing'.
reactimateMaybe ::
  (Functor m, Monad m) =>
  Automaton (MaybeT m) () () ->
  m ()
reactimateMaybe automaton = reactimateExcept $ try $ maybeToExceptS automaton

{- | Run an 'Automaton' fed from a list, discarding results. Useful when one needs to
combine effects and streams (i.e., for testing purposes).
-}
embed_ :: (Functor m, Monad m) => Automaton m a () -> [a] -> m ()
embed_ automaton as = reactimateMaybe $ listToMaybeS as >>> liftS automaton
