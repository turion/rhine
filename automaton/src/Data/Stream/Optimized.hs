{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{- | An optimization layer on "Data.Stream".

Since both variants are semantically the same, not the full API of "Data.Stream" is replicated here.
-}
module Data.Stream.Optimized where

-- base
import Control.Applicative (Alternative (..), Applicative (..), liftA2)
import Data.Monoid (Ap (..))
import Prelude hiding (Applicative (..))

-- transformers
import Control.Monad.Trans.Except (ExceptT)

-- selective
import Control.Selective (Selective (select))

-- simple-affine-space
import Data.VectorSpace

-- mmorph
import Control.Monad.Morph

-- align
import Data.Align (Align, Semialign)
import Data.Semialign (Align (..), Semialign (..))

-- automaton
import Data.Stream hiding (hoist')
import Data.Stream qualified as StreamT
import Data.Stream.Recursive (Recursive (..))
import Data.Stream.Result

{- | An optimized version of 'StreamT' which has an extra constructor for stateless streams.

In most cases, using 'OptimizedStreamT' is preferable over 'StreamT',
because building up bigger programs with 'StreamT' will build up big accumulations of trivial states.
The API of 'OptimizedStreamT' only keeps the nontrivial parts of the state.

Semantically, both types are the same.
-}
data OptimizedStreamT m a
  = -- | Embed a 'StreamT'. Take care only to use this constructor on streams with nontrivial state.
    Stateful (StreamT m a)
  | -- | A stateless stream is simply an action in a monad which is performed repetitively.
    Stateless (m a)
  deriving (Functor, Foldable, Traversable)

{- | Remove the optimization layer.

For stateful streams, this is just the identity.
A stateless stream is encoded as a stream with state '()'.
-}
toStreamT :: (Functor m) => OptimizedStreamT m b -> StreamT m b
toStreamT (Stateful stream) = stream
toStreamT (Stateless m) = StreamT {state = (), step = const $ Result () <$> m}
{-# INLINE toStreamT #-}

-- | Only builds up tuples of states if both streams are stateful.
instance (Applicative m) => Applicative (OptimizedStreamT m) where
  pure = Stateless . pure
  {-# INLINE pure #-}

  Stateful stream1 <*> Stateful stream2 = Stateful $ stream1 <*> stream2
  Stateless m <*> Stateful (StreamT state0 step) = Stateful $ StreamT state0 $ \state -> fmap . ($) <$> m <*> step state
  Stateful (StreamT state0 step) <*> Stateless m = Stateful $ StreamT state0 $ \state -> flip (fmap . flip ($)) <$> step state <*> m
  Stateless mf <*> Stateless ma = Stateless $ mf <*> ma
  {-# INLINE (<*>) #-}

deriving via Ap (OptimizedStreamT m) a instance (Applicative m, Num a) => Num (OptimizedStreamT m a)

instance (Applicative m, Fractional a) => Fractional (OptimizedStreamT m a) where
  fromRational = pure . fromRational
  recip = fmap recip

instance (Applicative m, Floating a) => Floating (OptimizedStreamT m a) where
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

instance (VectorSpace v s, Eq s, Floating s, Applicative m) => VectorSpace (OptimizedStreamT m v) (OptimizedStreamT m s) where
  zeroVector = pure zeroVector
  (*^) = liftA2 (*^)
  (^+^) = liftA2 (^+^)
  dot = liftA2 dot
  normalize = fmap normalize

instance (Alternative m) => Alternative (OptimizedStreamT m) where
  empty = Stateless empty
  {-# INLINE empty #-}

  -- The semantics prescribe that we save the state which stream was selected.
  stream1 <|> stream2 = Stateful $ toStreamT stream1 <|> toStreamT stream2
  {-# INLINE (<|>) #-}

  many stream = Stateful $ many $ toStreamT stream
  {-# INLINE many #-}

  some stream = Stateful $ some $ toStreamT stream
  {-# INLINE some #-}

instance (Selective m) => Selective (OptimizedStreamT m) where
  select (Stateless mab) (Stateless f) = Stateless $ select mab f
  select stream1 stream2 = Stateful $ select (toStreamT stream1) (toStreamT stream2)

instance (Semialign m) => Semialign (OptimizedStreamT m) where
  align (Stateless ma) (Stateless mb) = Stateless $ align ma mb
  align stream1 stream2 = Stateful $ align (toStreamT stream1) (toStreamT stream2)

instance (Align m) => Align (OptimizedStreamT m) where
  nil = Stateless nil

instance MFunctor OptimizedStreamT where
  hoist = hoist'
  {-# INLINE hoist #-}

-- | Like 'hoist', but without the @'Monad' m2@ constraint.
hoist' :: (forall x. m1 x -> m2 x) -> OptimizedStreamT m1 a -> OptimizedStreamT m2 a
hoist' f (Stateful stream) = Stateful $ StreamT.hoist' f stream
hoist' f (Stateless m) = Stateless $ f m
{-# INLINE hoist' #-}

-- | Change the output type and effect of a stream without changing its state type.
mapOptimizedStreamT :: (Functor m, Functor n) => (forall s. m (Result s a) -> n (Result s b)) -> OptimizedStreamT m a -> OptimizedStreamT n b
mapOptimizedStreamT f (Stateful stream) = Stateful $ withStreamT f stream
mapOptimizedStreamT f (Stateless m) = Stateless $ fmap output $ f $ fmap (Result ()) m
{-# INLINE mapOptimizedStreamT #-}

{- | Map a monad-independent morphism of streams to optimized streams.

In contrast to 'handleOptimized', the stream morphism must be independent of the monad.
-}
withOptimized :: (Monad n) => (forall m. (Monad m) => StreamT m a -> StreamT m b) -> OptimizedStreamT n a -> OptimizedStreamT n b
withOptimized f stream = Stateful $ f $ toStreamT stream

-- | Like 'withOptimized', but with fewer constraints.
withOptimizedF :: (Functor n) => (forall m. (Functor m) => StreamT m a -> StreamT m b) -> OptimizedStreamT n a -> OptimizedStreamT n b
withOptimizedF f stream = Stateful $ f $ toStreamT stream

{- | Map a morphism of streams to optimized streams.

In contrast to 'withOptimized', the monad type is allowed to change.
-}
handleOptimized :: (Functor m) => (StreamT m a -> StreamT n b) -> OptimizedStreamT m a -> OptimizedStreamT n b
handleOptimized f stream = Stateful $ f $ toStreamT stream

{- | Run a stream with trivial output.

See 'Data.Stream.reactimate'.
-}
reactimate :: (Monad m) => OptimizedStreamT m () -> m void
reactimate (Stateful stream) = StreamT.reactimate stream
reactimate (Stateless f) = go
  where
    go = f *> go
{-# INLINE reactimate #-}

{- | A stateless stream.

This function is typically preferable over 'Data.Stream.constM',
since the optimized version doesn't create a state type.
-}
constM :: m a -> OptimizedStreamT m a
constM = Stateless
{-# INLINE constM #-}

-- | Perform one step of a stream, resulting in an updated stream and an output value.
stepOptimizedStream :: (Functor m) => OptimizedStreamT m a -> m (Result (OptimizedStreamT m a) a)
stepOptimizedStream (Stateful stream) = mapResultState Stateful <$> stepStream stream
stepOptimizedStream oa@(Stateless m) = Result oa <$> m
{-# INLINE stepOptimizedStream #-}

{- | Translate to the recursive encoding of streams.

This will typically be a performance penalty.
-}
toRecursive :: (Functor m) => OptimizedStreamT m a -> Recursive m a
toRecursive (Stateful stream) = StreamT.toRecursive stream
toRecursive (Stateless f) = go
  where
    go = Recursive $ Result go <$> f
{-# INLINE toRecursive #-}

{- | Translate a stream from recursive encoding to stateful, coalgebraic encoding.
  The internal state is the stream itself.
-}
fromRecursive :: Recursive m a -> OptimizedStreamT m a
fromRecursive = Stateful . StreamT.fromRecursive
{-# INLINE fromRecursive #-}

-- | See 'Data.Stream.concatS'.
concatS :: (Monad m) => OptimizedStreamT m [a] -> OptimizedStreamT m a
concatS stream = Stateful $ StreamT.concatS $ toStreamT stream
{-# INLINE concatS #-}

-- | See 'Data.Stream.exceptS'.
exceptS :: (Monad m) => OptimizedStreamT (ExceptT e m) b -> OptimizedStreamT m (Either e b)
exceptS stream = Stateful $ StreamT.exceptS $ toStreamT stream
{-# INLINE exceptS #-}

-- | See 'Data.Stream.applyExcept'.
applyExcept :: (Monad m) => OptimizedStreamT (ExceptT (e1 -> e2) m) a -> OptimizedStreamT (ExceptT e1 m) a -> OptimizedStreamT (ExceptT e2 m) a
applyExcept streamF streamA = Stateful $ StreamT.applyExcept (toStreamT streamF) (toStreamT streamA)
{-# INLINE applyExcept #-}

-- | See 'Data.Stream.selectExcept'.
selectExcept :: (Monad m) => OptimizedStreamT (ExceptT (Either e1 e2) m) a -> OptimizedStreamT (ExceptT (e1 -> e2) m) a -> OptimizedStreamT (ExceptT e2 m) a
selectExcept streamE streamF = Stateful $ StreamT.selectExcept (toStreamT streamE) (toStreamT streamF)
{-# INLINE selectExcept #-}
