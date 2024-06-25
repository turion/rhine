{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Stream where

-- base
import Control.Applicative (Alternative (..), Applicative (..), liftA2)
import Control.Monad ((<$!>))
import Data.Bifunctor (bimap)
import Data.Monoid (Ap (..))
import Prelude hiding (Applicative (..))

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, withExceptT)

-- mmorph
import Control.Monad.Morph (MFunctor (hoist))

-- simple-affine-space
import Data.VectorSpace (VectorSpace (..))

-- selective
import Control.Selective

-- these
import Data.These (These (..))

-- semialign
import Data.Align

-- automaton
import Data.Stream.Internal
import Data.Stream.Result

-- * Creating streams

{- | Effectful streams in coalgebraic encoding.

A stream consists of an internal state @s@, and a step function.
This step can make use of an effect in @m@ (which is often a monad),
alter the state, and return a result value.
Its semantics is continuously outputting values of type @b@,
while performing side effects in @m@.

A coalgebraic encoding was chosen instead of the direct recursion known from e.g. @list-transformer@, @dunai@, @machines@, @streaming@, ...,
because the coalgebraic encoding is much more amenable to compiler optimizations
than the coalgebraic encoding, which is:

@
  data StreamRecursiveT m b = StreamRecursiveT (m (b, StreamRecursiveT m b))
@

When two streams are composed, GHC can often optimize the combined step function,
resulting in a faster streams than what the coalgebraic encoding can ever achieve,
because the coalgebraic encoding has to step through every continuation.
Put differently, the compiler can perform static analysis on the state types of initially encoded state machines,
while the coalgebraic encoding knows its state only at runtime.

This performance gain comes at a peculiar cost:
Recursive definitions /of/ streams are not possible, e.g. an equation like:
@
  fixA stream = stream <*> fixA stream
@
This is impossible since the stream under definition itself appears in the definition body,
and thus the internal /state type/ would be recursively defined, which GHC doesn't allow:
Type level recursion is not supported in existential types.
An stream defined thusly will typically hang and/or leak memory, trying to build up an infinite type at runtime.

It is nevertheless possible to define streams recursively, but one needs to first identify the recursive definition of its /state type/.
Then for the greatest generality, 'fixStream' and 'fixStream'' can be used, and some special cases are covered by functions
such as 'fixA', 'Data.Automaton.parallely', 'many' and 'some'.
-}
data StreamT m a
  = forall s.
  StreamT
  { state :: s
  -- ^ The internal state of the stream
  , step :: s -> m (Result s a)
  -- ^ Stepping a stream by one tick means:
  --   1. performing a side effect in @m@
  --   2. updating the internal state @s@
  --   3. outputting a value of type @a@
  }

-- | Initialise with an internal state, update the state and produce output without side effects.
unfold :: (Applicative m) => s -> (s -> Result s a) -> StreamT m a
unfold state step =
  StreamT
    { state
    , step = pure . step
    }

-- | Like 'unfold', but output the current state.
unfold_ :: (Applicative m) => s -> (s -> s) -> StreamT m s
unfold_ state step = unfold state $ \s -> let s' = step s in Result s' s'

-- | Constantly perform the same effect, without remembering a state.
constM :: (Functor m) => m a -> StreamT m a
constM ma = StreamT () $ const $ Result () <$> ma
{-# INLINE constM #-}

-- | Call the monadic action once on the first tick and provide its result indefinitely.
initialised :: (Monad m) => m a -> StreamT m a
initialised action =
  let step mr@(Just r) = pure $! Result mr r
      step Nothing = (step . Just =<< action)
   in StreamT
        { state = Nothing
        , step
        }
{-# INLINE initialised #-}

instance (Functor m) => Functor (StreamT m) where
  fmap f StreamT {state, step} = StreamT state $! fmap (fmap f) <$> step
  {-# INLINE fmap #-}

-- | 'pure' forever returns the same value, '(<*>)' steps two streams synchronously.
instance (Applicative m) => Applicative (StreamT m) where
  pure = constM . pure
  {-# INLINE pure #-}

  StreamT stateF0 stepF <*> StreamT stateA0 stepA =
    StreamT (JointState stateF0 stateA0) (\(JointState stateF stateA) -> apResult <$> stepF stateF <*> stepA stateA)
  {-# INLINE (<*>) #-}

deriving via Ap (StreamT m) a instance (Applicative m, Num a) => Num (StreamT m a)

instance (Applicative m, Fractional a) => Fractional (StreamT m a) where
  fromRational = pure . fromRational
  recip = fmap recip

instance (Applicative m, Floating a) => Floating (StreamT m a) where
  pi = pure pi
  exp = fmap exp
  log = fmap log
  sin = fmap sin
  cos = fmap cos
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh

instance (VectorSpace v s, Eq s, Floating s, Applicative m) => VectorSpace (StreamT m v) (StreamT m s) where
  zeroVector = pure zeroVector
  (*^) = liftA2 (*^)
  (^+^) = liftA2 (^+^)
  dot = liftA2 dot
  normalize = fmap normalize

{- | 'empty' just performs 'empty' in the underlying monad @m@.
  @s1 '<|>' s2@ starts in an undecided state,
  and explores the possibilities of continuing in @s1@ or @s2@
  on the first tick, using the underlying @m@.
-}
instance (Alternative m) => Alternative (StreamT m) where
  empty = constM empty
  {-# INLINE empty #-}

  StreamT stateL0 stepL <|> StreamT stateR0 stepR =
    StreamT
      { state = Undecided
      , step = \case
          Undecided -> (mapResultState DecideL <$> stepL stateL0) <|> (mapResultState DecideR <$> stepR stateR0)
          DecideL stateL -> mapResultState DecideL <$> stepL stateL
          DecideR stateR -> mapResultState DecideR <$> stepR stateR
      }
  {-# INLINE (<|>) #-}

  many StreamT {state, step} = fixStream'
    (const NotStarted)
    $ \fixstate fixstep -> \case
      NotStarted -> ((\(Result s' a) (Result ss' as) -> Result (Ongoing ss' s') $ a : as) <$> step state <*> fixstep fixstate) <|> pure (Result Finished [])
      Finished -> pure $! Result Finished []
      Ongoing ss s -> (\(Result s' a) (Result ss' as) -> Result (Ongoing ss' s') $ a : as) <$> step s <*> fixstep ss
  {-# INLINE many #-}

  some stream = (:) <$> stream <*> many stream
  {-# INLINE some #-}

instance MFunctor StreamT where
  hoist = hoist'
  {-# INLINE hoist #-}

{- | Hoist a stream along a monad morphism, by applying said morphism to the step function.

This is like @mmorph@'s 'hoist', but it doesn't require a 'Monad' constraint on @m2@.
-}
hoist' :: (forall x. m1 x -> m2 x) -> StreamT m1 a -> StreamT m2 a
hoist' f StreamT {state, step} = StreamT {state, step = f <$> step}
{-# INLINE hoist' #-}

-- * Running streams

-- | Perform one step of a stream, resulting in an updated stream and an output value.
stepStream :: (Functor m) => StreamT m a -> m (Result (StreamT m a) a)
stepStream StreamT {state, step} = mapResultState (`StreamT` step) <$> step state
{-# INLINE stepStream #-}

{- | Run a stream with trivial output.

If the output of a stream does not contain information,
all of its meaning is in its effects.
This function runs the stream indefinitely.
Since it will never return with a value, this function also has no output (its output is void).
The only way it can return is if @m@ includes some effect of termination,
e.g. 'Maybe' or 'Either' could terminate with a 'Nothing' or 'Left' value,
or 'IO' can raise an exception.
-}
reactimate :: (Monad m) => StreamT m () -> m void
reactimate StreamT {state, step} = go state
  where
    go s = do
      Result s' () <- step s
      go s'
{-# INLINE reactimate #-}

-- | Run a stream, collecting the outputs in a lazy, infinite list.
streamToList :: (Monad m) => StreamT m a -> m [a]
streamToList StreamT {state, step} = go state
  where
    go s = do
      Result s' a <- step s
      (a :) <$> go s'
{-# INLINE streamToList #-}

-- * Modifying streams

-- | Change the output type and effect of a stream without changing its state type.
withStreamT :: (Functor m, Functor n) => (forall s. m (Result s a) -> n (Result s b)) -> StreamT m a -> StreamT n b
withStreamT f StreamT {state, step} = StreamT state $ fmap f step
{-# INLINE withStreamT #-}

{- | Buffer the output of a stream, returning one value at a time.

This function lets a stream control the speed at which it produces data,
since it can decide to produce any amount of output at every step.
-}
concatS :: (Monad m) => StreamT m [a] -> StreamT m a
concatS StreamT {state, step} =
  StreamT
    { state = (state, [])
    , step = go
    }
  where
    go (s, []) = do
      Result s' as <- step s
      go (s', as)
    go (s, a : as) = pure $ Result (s, as) a
{-# INLINE concatS #-}

-- ** Exception handling

{- | Streams with exceptions are 'Applicative' in the exception type.

Run the first stream until it throws a function as an exception,
  then run the second one. If the second one ever throws an exception,
  apply the function thrown by the first one to it.
-}
applyExcept :: (Monad m) => StreamT (ExceptT (e1 -> e2) m) a -> StreamT (ExceptT e1 m) a -> StreamT (ExceptT e2 m) a
applyExcept (StreamT state1 step1) (StreamT state2 step2) =
  StreamT
    { state = Left state1
    , step
    }
  where
    step (Left s1) = do
      resultOrException <- lift $ runExceptT $ step1 s1
      case resultOrException of
        Right result -> pure $! mapResultState Left result
        Left f -> step (Right (state2, f))
    step (Right (s2, f)) = mapResultState (Right . (,f)) <$!> withExceptT f (step2 s2)
{-# INLINE applyExcept #-}

{- | Execute the stream until it throws an exception, then restart it.

One might be tempted to define this function recursively with 'applyExcept',
but this would result in a runtime error, trying to define an infinite state.
-}
foreverExcept :: (Functor m, Monad m) => StreamT (ExceptT e m) a -> StreamT m a
foreverExcept StreamT {state, step} =
  StreamT
    { state
    , step = stepNew
    }
  where
    stepNew s = do
      resultOrException <- runExceptT $ step s
      case resultOrException of
        Left _ -> stepNew state
        Right result -> pure result

-- | Whenever an exception occurs, output it and retry on the next step.
exceptS :: (Applicative m) => StreamT (ExceptT e m) b -> StreamT m (Either e b)
exceptS StreamT {state, step} =
  StreamT
    { step = \state -> fmap (either (Result state . Left) (fmap Right)) $ runExceptT $ step state
    , state
    }
{-# INLINE exceptS #-}

{- | Run the first stream until it throws an exception.
  If the exception is 'Right', throw it immediately.
  If it is 'Left', run the second stream until it throws a function, which is then applied to the first exception.
-}
selectExcept :: (Monad m) => StreamT (ExceptT (Either e1 e2) m) a -> StreamT (ExceptT (e1 -> e2) m) a -> StreamT (ExceptT e2 m) a
selectExcept (StreamT stateE0 stepE) (StreamT stateF0 stepF) =
  StreamT
    { state = Left stateE0
    , step
    }
  where
    step (Left stateE) = do
      resultOrException <- lift $ runExceptT $ stepE stateE
      case resultOrException of
        Right result -> pure $ mapResultState Left result
        Left (Left e1) -> step (Right (e1, stateF0))
        Left (Right e2) -> throwE e2
    step (Right (e1, stateF)) = withExceptT ($ e1) $ mapResultState (Right . (e1,)) <$> stepF stateF

instance (Selective m) => Selective (StreamT m) where
  select (StreamT stateE0 stepE) (StreamT stateF0 stepF) =
    StreamT
      { state = JointState stateE0 stateF0
      , step = \(JointState stateE stateF) ->
          (fmap (mapResultState (`JointState` stateF)) . eitherResult <$> stepE stateE)
            <*? ((\(Result stateF' f) (Result stateE' a) -> Result (JointState stateE' stateF') (f a)) <$> stepF stateF)
      }
    where
      eitherResult :: Result s (Either a b) -> Either (Result s a) (Result s b)
      eitherResult (Result s eab) = bimap (Result s) (Result s) eab

instance (Semialign m) => Semialign (StreamT m) where
  align (StreamT s10 step1) (StreamT s20 step2) =
    StreamT
      { state = These s10 s20
      , step = \case
          This s1 -> mapResultState This . fmap This <$> step1 s1
          That s2 -> mapResultState That . fmap That <$> step2 s2
          These s1 s2 -> commuteTheseResult <$> align (step1 s1) (step2 s2)
      }
    where
      commuteTheseResult :: These (Result s1 a1) (Result s2 a2) -> Result (These s1 s2) (These a1 a2)
      commuteTheseResult (This (Result s1 a1)) = Result (This s1) (This a1)
      commuteTheseResult (That (Result s2 a2)) = Result (That s2) (That a2)
      commuteTheseResult (These (Result s1 a1) (Result s2 a2)) = Result (These s1 s2) (These a1 a2)
  {-# INLINE align #-}

instance (Align m) => Align (StreamT m) where
  nil = constM nil
  {-# INLINE nil #-}

-- ** Fix points, or recursive definitions

{- | Recursively define a stream from a recursive definition of the state, and of the step function.

If you want to define a stream recursively, this is not possible directly.
For example, consider this definition:
@
loops :: Monad m => StreamT m [Int]
loops = (:) <$> unfold_ 0 (+ 1) <*> loops
@
The defined value @loops@ contains itself in its definition.
This means that the internal state type of @loops@ must itself be recursively defined.
But GHC cannot do this automatically, because type level and value level are separate.
Instead, we need to spell out the type level recursion explicitly with a type constructor,
over which we will take the fixpoint.

In this example, we can figure out from the definitions that:
1. @'unfold_' 0 (+ 1)@ has @0 :: Int@ as state
2. '(:)' does not change the state
3. '<*>' takes the product of both states

So the internal state @s@ of @loops@ must satisfy the equation @s = (Int, s)@.
If the recursion is written as above, it tries to compute the infinite tuple @(Int, (Int, (Int, ...)))@, which hangs.
Instead, we need to define a type operator over which we take the fixpoint:

@
-- You need to write this:
data Loops x = Loops Int x

-- The library supplies:
data Fix f = Fix f (Fix f)
type LoopsState = Fix Loops
@

We can then use 'fixStream' to define the recursive definition of @loops@.
For this, we have to to tediously inline the definitions of 'unfold_', '(:)', and '<*>',
until we arrive at an explicit recursive definition of the state and the step function of @loops@, separately.
These are the two arguments of 'fixStream'.

@
loops :: Monad m => StreamT m [Int]
loops = fixStream (Loops 0) $ \fixStep (Loops n fixState) -> do
  Result s' a <- fixStep fixState
  return $ Result (Loops (n + 1) s') a
@
-}
fixStream ::
  (Functor m) =>
  -- | The recursive definition of the state of the stream.
  (forall s. s -> t s) ->
  -- | The recursive definition of the step function of the stream.
  ( forall s.
    (s -> m (Result s a)) ->
    (t s -> m (Result (t s) a))
  ) ->
  StreamT m a
fixStream transformState transformStep =
  StreamT
    { state = fixState transformState
    , step
    }
  where
    step Fix {getFix} = mapResultState Fix <$> transformStep step getFix

-- | A generalisation of 'fixStream' where the step definition is allowed to depend on the state.
fixStream' ::
  (Functor m) =>
  (forall s. s -> t s) ->
  -- | The recursive definition of the state of the stream.
  (forall s. s -> (s -> m (Result s a)) -> (t s -> m (Result (t s) a))) ->
  -- | The recursive definition of the step function of the stream.
  StreamT m a
fixStream' transformState transformStep =
  StreamT
    { state = fixState transformState
    , step
    }
  where
    step fix@(Fix {getFix}) = mapResultState Fix <$> transformStep fix step getFix

{- | The solution to the equation @'fixA stream = stream <*> 'fixA' stream@.

Such a fix point operator needs to be used instead of the above direct definition because recursive definitions of streams
loop at runtime due to the coalgebraic encoding of the state.
-}
fixA :: (Applicative m) => StreamT m (a -> a) -> StreamT m a
fixA StreamT {state, step} = fixStream (JointState state) $
  \stepA (JointState s ss) -> apResult <$> step s <*> stepA ss
