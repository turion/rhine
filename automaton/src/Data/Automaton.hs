{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Automaton where

-- base
import Control.Applicative (Alternative (..))
import Control.Arrow
import Control.Category
import Control.Monad ((<=<))
import Control.Monad.Fix (MonadFix (mfix))
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose (..))
import Data.Maybe (fromMaybe)
import Data.Monoid (First (..), Sum (..))
import Prelude hiding (id, (.))

-- mmorph
import Control.Monad.Morph (MFunctor (..))

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

-- profunctors
import Data.Profunctor (Choice (..), Profunctor (..), Strong)
import Data.Profunctor.Strong (Strong (..))
import Data.Profunctor.Traversing

-- selective
import Control.Selective (Selective)

-- simple-affine-space
import Data.VectorSpace (VectorSpace (..))

-- align
import Data.Semialign (Align (..), Semialign (..))

-- automaton
import Data.Stream (StreamT (..), fixStream)
import Data.Stream qualified as StreamT
import Data.Stream.Internal (JointState (..))
import Data.Stream.Optimized (
  OptimizedStreamT (..),
  concatS,
  stepOptimizedStream,
 )
import Data.Stream.Optimized qualified as StreamOptimized
import Data.Stream.Result

-- * Constructing automata

{- | An effectful automaton in coalgebraic encoding.

* @m@: The monad in which the automaton performs side effects.
* @a@: The type of inputs the automaton constantly consumes.
* @b@: The type of outputs the automaton constantly produces.

An effectful automaton with input @a@ is the same as an effectful stream
with the additional effect of reading an input value @a@ on every step.
This is why automata are defined here as streams.

The API of automata follows that of streams ('StreamT' and 'OptimizedStreamT') closely.
The prominent addition in automata is now that they are instances of the 'Category', 'Arrow', 'Profunctor',
and related type classes.
This allows for more ways of creating or composing them.

For example, you can sequentially and parallely compose two automata:
@
automaton1 :: Automaton m a b
automaton2 :: Automaton m b c

sequentially :: Automaton m a c
sequentially = automaton1 >>> automaton2

parallely :: Automaton m (a, b) (b, c)
parallely = automaton1 *** automaton2
@
In sequential composition, the output of the first automaton is passed as input to the second one.
In parallel composition, both automata receive input simulataneously and process it independently.

Through the 'Arrow' type class, you can use 'arr' to create an automaton from a pure function,
and more generally use the arrow syntax extension to define automata.
-}
newtype Automaton m a b = Automaton {getAutomaton :: OptimizedStreamT (ReaderT a m) b}
  deriving newtype (Functor, Applicative, Alternative, Selective, Num, Fractional, Floating)

-- | Create an 'Automaton' from a state and a pure step function.
unfold ::
  (Applicative m) =>
  -- | The initial state
  s ->
  -- | The step function
  (a -> s -> Result s b) ->
  Automaton m a b
unfold state step = unfoldM state $ fmap pure <$> step

-- | Create an 'Automaton' from a state and an effectful step function.
unfoldM ::
  -- | The initial state
  s ->
  -- | The step function
  (a -> s -> m (Result s b)) ->
  Automaton m a b
unfoldM state step = Automaton $! Stateful $! StreamT {state, step = \s -> ReaderT $ \a -> step a s}

-- | Like 'unfold', but output the current state.
unfold_ ::
  (Applicative m) =>
  -- | The initial state
  s ->
  -- | The step function
  (a -> s -> s) ->
  Automaton m a s
unfold_ state step = unfold state $ \a s -> let s' = step a s in Result s' s'

instance (Eq s, Floating s, VectorSpace v s, Applicative m) => VectorSpace (Automaton m a v) (Automaton m a s) where
  zeroVector = Automaton zeroVector
  Automaton s *^ Automaton v = coerce $ s *^ v
  Automaton v1 ^+^ Automaton v2 = coerce $ v1 ^+^ v2
  dot (Automaton s) (Automaton v) = coerce $ dot s v
  normalize (Automaton v) = coerce v

instance (Semialign m) => Semialign (Automaton m a) where
  align automaton1 automaton2 =
    Automaton $
      StreamOptimized.hoist' (ReaderT . getCompose) $
        align
          (StreamOptimized.hoist' (Compose . runReaderT) $ getAutomaton automaton1)
          (StreamOptimized.hoist' (Compose . runReaderT) $ getAutomaton automaton2)

instance (Align m) => Align (Automaton m a) where
  nil = constM nil

instance (Monad m) => Category (Automaton m) where
  id = Automaton $ Stateless ask
  {-# INLINE id #-}

  Automaton (Stateful (StreamT stateF0 stepF)) . Automaton (Stateful (StreamT stateG0 stepG)) =
    Automaton $!
      Stateful $!
        StreamT
          { state = JointState stateF0 stateG0
          , step = \(JointState stateF stateG) -> do
              Result stateG' b <- stepG stateG
              Result stateF' c <- lift $! runReaderT (stepF stateF) b
              return $! Result (JointState stateF' stateG') c
          }
  Automaton (Stateful (StreamT state0 step)) . Automaton (Stateless m) =
    Automaton $!
      Stateful $!
        StreamT
          { state = state0
          , step = \state -> do
              b <- m
              lift $! runReaderT (step state) b
          }
  Automaton (Stateless m) . Automaton (Stateful (StreamT state0 step)) =
    Automaton $!
      Stateful $!
        StreamT
          { state = state0
          , step = \state -> do
              Result state' b <- step state
              c <- lift $! runReaderT m b
              return $! Result state' c
          }
  Automaton (Stateless f) . Automaton (Stateless g) = Automaton $ Stateless $ ReaderT $ runReaderT f <=< runReaderT g
  {-# INLINE (.) #-}

instance (Monad m) => Arrow (Automaton m) where
  arr f = Automaton $! Stateless $! asks f
  {-# INLINE arr #-}

  first (Automaton (Stateful StreamT {state, step})) =
    Automaton $!
      Stateful $!
        StreamT
          { state
          , step = \s ->
              ReaderT
                ( \(b, d) ->
                    fmap (,d)
                      <$> runReaderT (step s) b
                )
          }
  first (Automaton (Stateless m)) = Automaton $ Stateless $ ReaderT $ \(b, d) -> (,d) <$> runReaderT m b
  {-# INLINE first #-}

instance (Monad m) => ArrowChoice (Automaton m) where
  Automaton (Stateful (StreamT stateL0 stepL)) +++ Automaton (Stateful (StreamT stateR0 stepR)) =
    Automaton $!
      Stateful $!
        StreamT
          { state = JointState stateL0 stateR0
          , step = \(JointState stateL stateR) ->
              ReaderT $!
                either
                  (runReaderT (mapResultState (`JointState` stateR) . fmap Left <$> stepL stateL))
                  (runReaderT (mapResultState (JointState stateL) . fmap Right <$> stepR stateR))
          }
  Automaton (Stateless m) +++ Automaton (Stateful (StreamT state0 step)) =
    Automaton $!
      Stateful $!
        StreamT
          { state = state0
          , step = \state ->
              ReaderT $!
                either
                  (runReaderT . fmap (Result state . Left) $ m)
                  (runReaderT . fmap (fmap Right) $ step state)
          }
  Automaton (Stateful (StreamT state0 step)) +++ Automaton (Stateless m) =
    Automaton $!
      Stateful $!
        StreamT
          { state = state0
          , step = \state ->
              ReaderT $!
                either
                  (runReaderT . fmap (fmap Left) $ step state)
                  (runReaderT . fmap (Result state . Right) $ m)
          }
  Automaton (Stateless mL) +++ Automaton (Stateless mR) =
    Automaton $
      Stateless $
        ReaderT $
          either
            (runReaderT . fmap Left $ mL)
            (runReaderT . fmap Right $ mR)
  {-# INLINE (+++) #-}

  left (Automaton (Stateful (StreamT {state, step}))) =
    Automaton $!
      Stateful $!
        StreamT
          { state
          , step = \s -> ReaderT $ either (fmap (fmap Left) . runReaderT (step s)) (pure . Result s . Right)
          }
  left (Automaton (Stateless ma)) = Automaton $! Stateless $! ReaderT $! either (fmap Left . runReaderT ma) (pure . Right)
  {-# INLINE left #-}

  right (Automaton (Stateful (StreamT {state, step}))) =
    Automaton $!
      Stateful $!
        StreamT
          { state
          , step = \s -> ReaderT $ either (pure . Result s . Left) (fmap (fmap Right) . runReaderT (step s))
          }
  right (Automaton (Stateless ma)) = Automaton $! Stateless $! ReaderT $! either (pure . Left) (fmap Right . runReaderT ma)
  {-# INLINE right #-}

  f ||| g = f +++ g >>> arr untag
    where
      untag (Left x) = x
      untag (Right y) = y
  {-# INLINE (|||) #-}

-- | Caution, this can make your program hang. Try to use 'feedback' or 'unfold' where possible, or combine 'loop' with 'delay'.
instance (MonadFix m) => ArrowLoop (Automaton m) where
  loop (Automaton (Stateless ma)) = Automaton $! Stateless $! ReaderT (\b -> fst <$> mfix ((. snd) $ ($ b) $ curry $ runReaderT ma))
  loop (Automaton (Stateful (StreamT {state, step}))) =
    Automaton $!
      Stateful $!
        StreamT
          { state
          , step = \s -> ReaderT $ \b -> fmap fst <$> mfix ((. (snd . output)) $ ($ b) $ curry $ runReaderT $ step s)
          }
  {-# INLINE loop #-}

instance (Monad m, Alternative m) => ArrowZero (Automaton m) where
  zeroArrow = empty

instance (Monad m, Alternative m) => ArrowPlus (Automaton m) where
  (<+>) = (<|>)

-- | Consume an input and produce output effectfully, without keeping internal state
arrM :: (Functor m) => (a -> m b) -> Automaton m a b
arrM f = Automaton $! StreamOptimized.constM $! ReaderT f
{-# INLINE arrM #-}

-- | Produce output effectfully, without keeping internal state
constM :: (Functor m) => m b -> Automaton m a b
constM = arrM . const
{-# INLINE constM #-}

-- | Apply an arbitrary monad morphism to an automaton.
hoistS :: (Monad m) => (forall x. m x -> n x) -> Automaton m a b -> Automaton n a b
hoistS morph (Automaton automaton) = Automaton $ hoist (mapReaderT morph) automaton
{-# INLINE hoistS #-}

-- | Lift the monad of an automaton to a transformer.
liftS :: (MonadTrans t, Monad m, Functor (t m)) => Automaton m a b -> Automaton (t m) a b
liftS = hoistS lift
{-# INLINE liftS #-}

{- | Extend the internal state and feed back part of the output to the next input.

This is one of the fundamental ways to incorporate recursive dataflow in automata.
Given an automaton which consumes an additional input and produces an additional output,
the state of the automaton is extended by a further value.
This value is used as the additional input,
and the resulting additional output is stored in the internal state for the next step.
-}
feedback ::
  (Functor m) =>
  -- | The additional internal state
  c ->
  -- | The original automaton
  Automaton m (a, c) (b, c) ->
  Automaton m a b
feedback c (Automaton (Stateful StreamT {state, step})) =
  Automaton $!
    Stateful $!
      StreamT
        { state = JointState state c
        , step = \(JointState s c) -> ReaderT $ \a -> (\(Result s (b, c)) -> Result (JointState s c) b) <$> runReaderT (step s) (a, c)
        }
feedback state (Automaton (Stateless m)) =
  Automaton $!
    Stateful $!
      StreamT
        { state
        , step = \c -> ReaderT $ \a -> (\(b, c) -> Result c b) <$> runReaderT m (a, c)
        }
{-# INLINE feedback #-}

-- * Running automata

{- | Run one step of an automaton.

This consumes an input value, performs a side effect, and returns an updated automaton together with an output value.
-}
stepAutomaton :: (Functor m) => Automaton m a b -> a -> m (Result (Automaton m a b) b)
stepAutomaton (Automaton automatonT) a =
  runReaderT (stepOptimizedStream automatonT) a
    <&> mapResultState Automaton
{-# INLINE stepAutomaton #-}

{- | Run an automaton with trivial input and output indefinitely.

If the input and output of an automaton does not contain information,
all of its meaning is in its effects.
This function runs the automaton indefinitely.
Since it will never return with a value, this function also has no output (its output is void).
The only way it can return is if @m@ includes some effect of termination,
e.g. 'Maybe' or 'Either' could terminate with a 'Nothing' or 'Left' value,
or 'IO' can raise an exception.
-}
reactimate :: (Monad m) => Automaton m () () -> m void
reactimate (Automaton automaton) = StreamOptimized.reactimate $ hoist (`runReaderT` ()) automaton
{-# INLINE reactimate #-}

{- | Run an automaton with given input, for a given number of steps.

Especially for tests and batch processing,
it is useful to step an automaton with given input.
-}
embed ::
  (Monad m) =>
  -- | The automaton to run
  Automaton m a b ->
  -- | The input values
  [a] ->
  m [b]
embed (Automaton (Stateful StreamT {state, step})) = go state
  where
    go _s [] = return []
    go s (a : as) = do
      Result s' b <- runReaderT (step s) a
      (b :) <$> go s' as
embed (Automaton (Stateless m)) = mapM $ runReaderT m

-- * Modifying automata

-- | Change the input and output type and effect of an automaton without changing its state type.
withAutomaton :: (Functor m1, Functor m2) => (forall s. (a1 -> m1 (Result s b1)) -> (a2 -> m2 (Result s b2))) -> Automaton m1 a1 b1 -> Automaton m2 a2 b2
withAutomaton f = Automaton . StreamOptimized.mapOptimizedStreamT (ReaderT . f . runReaderT) . getAutomaton
{-# INLINE withAutomaton #-}

-- | Change the output type and effect of an automaton without changing its state type.
withAutomaton_ :: (Functor m1, Functor m2) => (forall s. m1 (Result s b1) -> m2 (Result s b2)) -> Automaton m1 a b1 -> Automaton m2 a b2
withAutomaton_ f = Automaton . StreamOptimized.mapOptimizedStreamT (mapReaderT f) . getAutomaton
{-# INLINE withAutomaton_ #-}

instance (Functor m) => Profunctor (Automaton m) where
  dimap f g Automaton {getAutomaton} = Automaton $ g <$> StreamOptimized.hoist' (withReaderT f) getAutomaton
  lmap f Automaton {getAutomaton} = Automaton $ StreamOptimized.hoist' (withReaderT f) getAutomaton
  rmap = fmap

instance (Monad m) => Choice (Automaton m) where
  right' = right
  left' = left

instance (Monad m) => Strong (Automaton m) where
  second' = second
  first' = first

-- | Step an automaton several steps at once, depending on how long the input is.
instance (Monad m) => Traversing (Automaton m) where
  wander f Automaton {getAutomaton = Stateful StreamT {state, step}} =
    Automaton
      { getAutomaton =
          Stateful
            StreamT
              { state
              , step =
                  step
                    & fmap runReaderT
                    & flip
                    & fmap ResultStateT
                    & f
                    & fmap getResultStateT
                    & flip
                    & fmap ReaderT
              }
      }
  wander f (Automaton (Stateless m)) = Automaton $ Stateless $ ReaderT $ f $ runReaderT m
  {-# INLINE wander #-}

-- | Only step the automaton if the input is 'Just'.
mapMaybeS :: (Monad m) => Automaton m a b -> Automaton m (Maybe a) (Maybe b)
mapMaybeS = traverse'

-- | Use an 'Automaton' with a variable amount of input.
traverseS :: (Monad m, Traversable f) => Automaton m a b -> Automaton m (f a) (f b)
traverseS = traverse'

-- | Like 'traverseS', discarding the output.
traverseS_ :: (Monad m, Traversable f) => Automaton m a b -> Automaton m (f a) ()
traverseS_ automaton = traverse' automaton >>> arr (const ())

{- | Launch arbitrarily many copies of the automaton in parallel.

* The copies of the automaton are launched on demand as the input lists grow.
* The n-th copy will always receive the n-th input.
* If the input list has length n, the n+1-th automaton copy will not be stepped.

Caution: Uses memory of the order of the largest list that was ever input during runtime.
-}
parallely :: (Applicative m) => Automaton m a b -> Automaton m [a] [b]
parallely Automaton {getAutomaton = Stateful stream} = Automaton $ Stateful $ parallely' stream
  where
    parallely' :: (Applicative m) => StreamT (ReaderT a m) b -> StreamT (ReaderT [a] m) [b]
    parallely' StreamT {state, step} = fixStream (JointState state) $ \fixstep jointState@(JointState s fixstate) -> ReaderT $ \case
      [] -> pure $! Result jointState []
      (a : as) -> apResult . fmap (:) <$> runReaderT (step s) a <*> runReaderT (fixstep fixstate) as
parallely Automaton {getAutomaton = Stateless f} = Automaton $ Stateless $ ReaderT $ traverse $ runReaderT f

-- | Given a transformation of streams, apply it to an automaton, without changing the input.
handleAutomaton_ :: (Monad m) => (forall m. (Monad m) => StreamT m a -> StreamT m b) -> Automaton m i a -> Automaton m i b
handleAutomaton_ f = Automaton . StreamOptimized.withOptimized f . getAutomaton

-- | Given a transformation of streams, apply it to an automaton. The input can be accessed through the 'ReaderT' effect.
handleAutomaton :: (Monad m) => (StreamT (ReaderT a m) b -> StreamT (ReaderT c n) d) -> Automaton m a b -> Automaton n c d
handleAutomaton f = Automaton . StreamOptimized.handleOptimized f . getAutomaton

{- | Buffer the output of an automaton. See 'Data.Stream.concatS'.

The input for the automaton is not buffered.
For example, if @'concatS' automaton@ receives one input @a@ and @automaton@ produces 10 @b@s from it,
then the next 9 inputs will be ignored.
-}
concatS :: (Monad m) => Automaton m a [b] -> Automaton m a b
concatS (Automaton automaton) = Automaton $ Data.Stream.Optimized.concatS automaton

-- * Examples

-- | Pass through a value unchanged, and perform a side effect depending on it
withSideEffect ::
  (Monad m) =>
  -- | For every value passing through the automaton, this function is called and the resulting side effect performed.
  (a -> m b) ->
  Automaton m a a
withSideEffect f = (id &&& arrM f) >>> arr fst

-- | Accumulate the input, output the accumulator.
accumulateWith ::
  (Monad m) =>
  -- | The accumulation function
  (a -> b -> b) ->
  -- | The initial accumulator
  b ->
  Automaton m a b
accumulateWith f state = unfold state $ \a b -> let b' = f a b in Result b' b'

{- | Like 'accumulateWith', with 'mappend' as the accumulation function.

The new values are 'mappend'ed from the left.
-}
mappendFrom :: (Monoid w, Monad m) => w -> Automaton m w w
mappendFrom = accumulateWith mappend

-- | Delay the input by one step.
delay ::
  (Applicative m) =>
  -- | The value to output on the first step
  a ->
  Automaton m a a
delay a0 = unfold a0 $ \aIn aState -> Result aIn aState

{- | Delay an automaton by one step by prepending one value to the output.

On the first step, the given initial output is returned.
On all subsequent steps, the automaton is stepped with the previous input.
-}
prepend :: (Monad m) => b -> Automaton m a b -> Automaton m a b
prepend b0 automaton = proc a -> do
  eab <- delay (Left b0) -< Right a
  case eab of
    Left b -> returnA -< b
    Right a -> automaton -< a

-- | Like 'mappendFrom', initialised at 'mempty'.
mappendS :: (Monoid w, Monad m) => Automaton m w w
mappendS = mappendFrom mempty

-- | Sum up all inputs so far, with an explicit initial value.
sumFrom :: (VectorSpace v s, Monad m) => v -> Automaton m v v
sumFrom = accumulateWith (^+^)

-- | Like 'sumFrom', initialised at 0.
sumS :: (Monad m, VectorSpace v s) => Automaton m v v
sumS = sumFrom zeroVector

-- | Sum up all inputs so far, initialised at 0.
sumN :: (Monad m, Num a) => Automaton m a a
sumN = arr Sum >>> mappendS >>> arr getSum
{-# INLINE sumN #-}

-- | Count the natural numbers, beginning at 1.
count :: (Num n, Monad m) => Automaton m a n
count = feedback 0 $! arr (\(_, n) -> let n' = n + 1 in (n', n'))
{-# INLINE count #-}

-- | Remembers the last 'Just' value, defaulting to the given initialisation value.
lastS :: (Monad m) => a -> Automaton m (Maybe a) a
-- From the naming, it's unintuitive that we use First and not Last,
-- but this is correct since mappendS appends from left.
lastS a = arr First >>> mappendS >>> arr (getFirst >>> fromMaybe a)
{-# INLINE lastS #-}

-- | Call the monadic action once on the first tick and provide its result indefinitely.
initialised :: (Monad m) => (a -> m b) -> Automaton m a b
initialised = Automaton . Stateful . StreamT.initialised . ReaderT
{-# INLINE initialised #-}

-- | Like 'initialised', but ignores the input.
initialised_ :: (Monad m) => m b -> Automaton m a b
initialised_ = initialised . const
{-# INLINE initialised_ #-}
