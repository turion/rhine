{-# LANGUAGE Arrows #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StrictData #-}

{- | An 'Automaton' in the 'ExceptT' monad can throw an exception to terminate.

This module defines several ways to throw exceptions,
and implements control flow by handling them.

The API is heavily inspired by @dunai@.
-}
module Data.Automaton.Trans.Except (
  module Data.Automaton.Trans.Except,
  module Control.Monad.Trans.Except,
)
where

-- base
import Control.Arrow (arr, returnA, (<<<), (>>>))
import Control.Category qualified as Category
import Data.Void (Void, absurd)

-- transformers
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Reader

-- selective
import Control.Selective (Selective)

-- mmorph
import Control.Monad.Morph

-- automaton
import Data.Automaton (
  Automaton (..),
  arrM,
  constM,
  count,
  feedback,
  hoistS,
  liftS,
  mapMaybeS,
  reactimate,
 )
import Data.Automaton.Trans.Except.Internal
import Data.Stream.Except hiding (safely)
import Data.Stream.Except qualified as StreamExcept
import Data.Stream.Optimized (mapOptimizedStreamT)
import Data.Stream.Optimized qualified as StreamOptimized

-- * Throwing exceptions

-- | Throw the exception 'e' whenever the function evaluates to 'True'.
throwOnCond :: (Monad m) => (a -> Bool) -> e -> Automaton (ExceptT e m) a a
throwOnCond cond e = proc a ->
  if cond a
    then throwS -< e
    else returnA -< a

{- | Throws the exception when the input is 'True'. Variant of 'throwOnCond'
for Kleisli arrows.
-}
throwOnCondM :: (Monad m) => (a -> m Bool) -> e -> Automaton (ExceptT e m) a a
throwOnCondM cond e = proc a -> do
  b <- arrM (lift . cond) -< a
  if b
    then throwS -< e
    else returnA -< a

-- | Throw the exception when the input is 'True'.
throwOn :: (Monad m) => e -> Automaton (ExceptT e m) Bool ()
throwOn e = proc b -> throwOn' -< (b, e)

-- | Variant of 'throwOn', where the exception may change every tick.
throwOn' :: (Monad m) => Automaton (ExceptT e m) (Bool, e) ()
throwOn' = proc (b, e) ->
  if b
    then throwS -< e
    else returnA -< ()

{- | When the input is @Just e@, throw the exception @e@.

This does not output any data since it terminates on the first nontrivial input.
-}
throwMaybe :: (Monad m) => Automaton (ExceptT e m) (Maybe e) (Maybe void)
throwMaybe = mapMaybeS throwS

{- | Immediately throw the incoming exception.

This is useful to combine with 'ArrowChoice',
e.g. with @if@ and @case@ expressions in Arrow syntax.
-}
throwS :: (Monad m) => Automaton (ExceptT e m) e a
throwS = arrM throwE

-- | Immediately throw the given exception.
throw :: (Monad m) => e -> Automaton (ExceptT e m) a b
throw = constM . throwE

-- | Do not throw an exception.
pass :: (Monad m) => Automaton (ExceptT e m) a a
pass = Category.id

{- | Converts an 'Automaton' in 'MaybeT' to an 'Automaton' in 'ExceptT'.

Whenever 'Nothing' is thrown, throw @()@ instead.
-}
maybeToExceptS ::
  (Functor m, Monad m) =>
  Automaton (MaybeT m) a b ->
  Automaton (ExceptT () m) a b
maybeToExceptS = hoistS (ExceptT . (maybe (Left ()) Right <$>) . runMaybeT)

-- * Catching exceptions

{- | Catch an exception in an 'Automaton'.

As soon as an exception occurs, switch to a new 'Automaton',
the exception handler, based on the exception value.

For exception catching where the handler can throw further exceptions, see 'AutomatonExcept' further below.
-}
catchS :: (Monad m) => Automaton (ExceptT e m) a b -> (e -> Automaton m a b) -> Automaton m a b
catchS automaton f = safely $ do
  e <- try automaton
  safe $ f e

-- | Similar to Yampa's delayed switching. Loses a @b@ in case of an exception.
untilE ::
  (Monad m) =>
  Automaton m a b ->
  Automaton m b (Maybe e) ->
  Automaton (ExceptT e m) a b
untilE automaton automatone = proc a -> do
  b <- liftS automaton -< a
  me <- liftS automatone -< b
  inExceptT -< ExceptT $ return $ maybe (Right b) Left me

{- | Escape an 'ExceptT' layer by outputting the exception whenever it occurs.

If an exception occurs, the current state is is tested again on the next input.
-}
exceptS :: (Functor m, Monad m) => Automaton (ExceptT e m) a b -> Automaton m a (Either e b)
exceptS = Automaton . StreamOptimized.exceptS . mapOptimizedStreamT commuteReader . getAutomaton

{- | Embed an 'ExceptT' value inside the 'Automaton'.

Whenever the input value is an ordinary value, it is passed on. If it is an exception, it is raised.
-}
inExceptT :: (Monad m) => Automaton (ExceptT e m) (ExceptT e m a) a
inExceptT = arrM id

{- | In case an exception occurs in the first argument, replace the exception
by the second component of the tuple.
-}
tagged :: (Monad m) => Automaton (ExceptT e1 m) a b -> Automaton (ExceptT e2 m) (a, e2) b
tagged automaton = runAutomatonExcept $ try (automaton <<< arr fst) *> (snd <$> currentInput)

-- * Monad interface for Exception Automatons

{- | An 'Automaton' that can terminate with an exception.

* @m@: The monad that the 'Automaton' may take side effects in.
* @a@: The type of input values the stream constantly consumes.
* @b@: The type of output values the stream constantly produces.
* @e@: The type of exceptions with which the stream can terminate.

This type is useful because it is a monad in the /exception type/ @e@.

  * 'return' corresponds to throwing an exception immediately.
  * '>>=' is exception handling: The first value throws an exception, while
    the Kleisli arrow handles the exception and produces a new signal
    function, which can throw exceptions in a different type.

Consider this example:
@
automaton :: AutomatonExcept m a b e1
f :: e1 -> AutomatonExcept m a b e2

example :: AutomatonExcept m a b e2
example = automaton >>= f
@

Here, @automaton@ produces output values of type @b@ until an exception @e1@ occurs.
The function @f@ is called on the exception value and produces a continuation automaton
which is then executed (until it possibly throws an exception @e2@ itself).

The generality of the monad interface comes at a cost, though.
In order to achieve higher performance, you should use the 'Monad' interface sparingly.
Whenever you can express the same control flow using 'Functor', 'Applicative', 'Selective',
or just the '(>>)' operator, you should do this.
The encoding of the internal state type will be much more efficiently optimized.

The reason for this is that in an expression @ma >>= f@,
the type of @f@ is @e1 -> AutomatonExcept m a b e2@,
which implies that the state of the 'AutomatonExcept' produced isn't known at compile time,
and thus GHC cannot optimize the automaton.
But often the full expressiveness of '>>=' isn't necessary, and in these cases,
a much faster automaton is produced by using 'Functor', 'Applicative' and 'Selective'.

Note: By "exceptions", we mean an 'ExceptT' transformer layer, not 'IO' exceptions.
-}
newtype AutomatonExcept m a b e = AutomatonExcept {getAutomatonExcept :: StreamExcept (ReaderT a m) b e}
  deriving newtype (Functor, Applicative, Selective, Monad)

runAutomatonExcept :: (Monad m) => AutomatonExcept m a b e -> Automaton (ExceptT e m) a b
runAutomatonExcept = Automaton . hoist commuteReaderBack . runStreamExcept . getAutomatonExcept

{- | Execute an 'Automaton' in 'ExceptT' until it raises an exception.

Typically used to enter the monad context of 'AutomatonExcept'.
-}
try :: (Monad m) => Automaton (ExceptT e m) a b -> AutomatonExcept m a b e
try = AutomatonExcept . InitialExcept . hoist commuteReader . getAutomaton

{- | Immediately throw the current input as an exception.

Useful inside 'AutomatonExcept' if you don't want to advance a further step in execution,
but first see what the current input is before continuing.
-}
currentInput :: (Monad m) => AutomatonExcept m e b e
currentInput = try throwS

{- | If no exception can occur, the 'Automaton' can be executed without the 'ExceptT'
layer.

Used to exit the 'AutomatonExcept' context, often in combination with 'safe':

@
automaton = safely $ do
  e <- try someAutomaton
  once $ \input -> putStrLn $ "Whoops, something happened when receiving input " ++ show input ++ ": " ++ show e ++ ", but I'll continue now."
  safe fallbackAutomaton
-}
safely :: (Monad m) => AutomatonExcept m a b Void -> Automaton m a b
safely = Automaton . StreamExcept.safely . getAutomatonExcept

{- | An 'Automaton' without an 'ExceptT' layer never throws an exception, and can
thus have an arbitrary exception type.

In particular, the exception type can be 'Void', so it can be used as the last statement in an 'AutomatonExcept' @do@-block.
See 'safely' for an example.
-}
safe :: (Monad m) => Automaton m a b -> AutomatonExcept m a b e
safe = try . liftS

{- | Inside the 'AutomatonExcept' monad, execute an action of the wrapped monad.
This passes the last input value to the action, but doesn't advance a tick.
-}
once :: (Monad m) => (a -> m e) -> AutomatonExcept m a b e
once f = AutomatonExcept $ InitialExcept $ StreamOptimized.constM $ ExceptT $ ReaderT $ fmap Left <$> f

-- | Variant of 'once' without input.
once_ :: (Monad m) => m e -> AutomatonExcept m a b e
once_ = once . const

-- | Advances a single tick with the given Kleisli arrow, and then throws an exception.
step :: (Monad m) => (a -> m (b, e)) -> AutomatonExcept m a b e
step f = try $ proc a -> do
  n <- count -< ()
  (b, e) <- arrM (lift . f) -< a
  _ <- throwOn' -< (n > (1 :: Int), e)
  returnA -< b

-- | Advances a single tick outputting the value, and then throws '()'.
step_ :: (Monad m) => b -> AutomatonExcept m a b ()
step_ b = step $ const $ return (b, ())

{- | Converts a list to an 'AutomatonExcept', which outputs an element of the list at
each step, throwing '()' when the list ends.
-}
listToAutomatonExcept :: (Monad m) => [b] -> AutomatonExcept m a b ()
listToAutomatonExcept = mapM_ step_

-- * Utilities definable in terms of 'AutomatonExcept'

{- | Extract an 'Automaton' from a monadic action.

Runs a monadic action that produces an 'Automaton' on the first step,
and then runs result for all further inputs (including the first one).
-}
performOnFirstSample :: (Monad m) => m (Automaton m a b) -> Automaton m a b
performOnFirstSample mAutomaton = safely $ do
  automaton <- once_ mAutomaton
  safe automaton

-- | 'reactimate's an 'AutomatonExcept' until it throws an exception.
reactimateExcept :: (Monad m) => AutomatonExcept m () () e -> m e
reactimateExcept ae = fmap (either id absurd) $ runExceptT $ reactimate $ runAutomatonExcept ae

-- | 'reactimate's an 'Automaton' until it returns 'True'.
reactimateB :: (Monad m) => Automaton m () Bool -> m ()
reactimateB ae = reactimateExcept $ try $ liftS ae >>> throwOn ()

{- | Run the first 'Automaton' until the second value in the output tuple is @Just c@,
then start the second automaton, discarding the current output @b@.

This is analogous to Yampa's
[@switch@](https://hackage.haskell.org/package/Yampa/docs/FRP-Yampa-Switches.html#v:switch),
with 'Maybe' instead of @Event@.
-}
switch :: (Monad m) => Automaton m a (b, Maybe c) -> (c -> Automaton m a b) -> Automaton m a b
switch automaton = catchS $ proc a -> do
  (b, me) <- liftS automaton -< a
  throwMaybe -< me
  returnA -< b

{- | Run the first 'Automaton' until the second value in the output tuple is @Just c@,
then start the second automaton one step later (after the current @b@ has been output).

Analog to Yampa's
[@dswitch@](https://hackage.haskell.org/package/Yampa/docs/FRP-Yampa-Switches.html#v:dSwitch),
with 'Maybe' instead of @Event@.
-}
dSwitch :: (Monad m) => Automaton m a (b, Maybe c) -> (c -> Automaton m a b) -> Automaton m a b
dSwitch sf = catchS $ feedback Nothing $ proc (a, me) -> do
  throwMaybe -< me
  liftS sf -< a
