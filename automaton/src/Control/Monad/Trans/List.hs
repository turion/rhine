{-# LANGUAGE DerivingStrategies #-}
module Control.Monad.Trans.List where

import Data.Stream.Except (StreamExcept (CoalgebraicExcept), mapOutput, stepInstant, runStreamExcept)
import Data.Coerce (coerce)
import Data.Stream.Optimized (OptimizedStreamT(..))
import Data.Stream (StreamT (..))
import Data.Stream.Result (Result(..))
import Control.Monad.Trans.Except (throwE, runExceptT)
import Control.Monad.Trans.Class (lift, MonadTrans)
import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus)
import Control.Monad.Morph (MFunctor (..))
import Data.Functor ((<&>))


newtype ListT m a = ListT {getListT :: StreamExcept a m ()}

-- FIXME functions from [m a]

instance Functor m => Functor (ListT m) where
  fmap = coerce mapOutput

-- FIXME It's weird I have so many basic constructions in this module where I can't reuse other code.
-- Probably there are some important utilities missing elsewhere

instance Monad m => Applicative (ListT m) where
  pure a = ListT $ CoalgebraicExcept $ Stateful $ StreamT
    { state = False
    , step = \done -> if done then throwE () else pure $! Result True a
    }


  ListT fs <*> ListT as = ap (runStreamExcept fs) (runStreamExcept as)
    where
      ap (Stateful (StreamT fState0 fStep)) (Stateful (StreamT aState0 aStep)) =   ListT $ CoalgebraicExcept $ Stateful StreamT
        -- FIXME strict datatype
        { state = (fState0, aState0, Nothing)
        , step
        }
         where
          -- FIXME Maybe this would work with Applicative m as well if I unroll a bit?
           step (fState, aState, Nothing) = do
              Result fState' f <- fStep fState
              step (fState', aState, Just f)
           step (fState, aState, justF@(Just f)) = do
              resultA <- lift $ runExceptT $ aStep aState
              case resultA of
                Left () -> step (fState, aState0, Nothing)
                Right (Result aState' a) -> return $! Result (fState, aState', justF) $ f a
      ap (Stateless mf) (Stateful (StreamT aState0 aStep)) = ListT $ CoalgebraicExcept $ Stateful StreamT
        { state = aState0
        , step = \aState -> do
            f <- mf
            Result aState' a <- aStep aState
            return $ Result aState' $ f a
        }
      ap (Stateful (StreamT fState0 fStep)) (Stateless ma) = ListT $ CoalgebraicExcept $ Stateful StreamT
        { state = fState0
        , step = \fState -> do
            Result fState' f <- fStep fState
            Result fState' . f <$> ma
            }
      ap (Stateless f) (Stateless a) = ListT $ CoalgebraicExcept $ Stateless $ f <*> a

instance Monad m => Monad (ListT m) where
  ListT startingCont >>= f = ListT $ go startingCont
   where
    go current = do
      maybeResult <- lift $ stepInstant current
      case maybeResult of
        Left () -> return ()
        Right (Result cont a) -> do
          getListT $ f a
          go cont

instance Monad m => Alternative (ListT m) where
  empty = ListT $ pure ()
  ListT as1 <|> ListT as2 = ListT $ as1 >> as2

instance Monad m => MonadPlus (ListT m)

-- FIXME Unclear which Monoid instance I'd want. liftA2 (<>) or <|>?

instance MonadTrans ListT where
  lift ma = ListT $ CoalgebraicExcept $ Stateful $ StreamT
    { state = False
    , step = \done -> if done then throwE () else Result True <$> lift ma
    }

instance MFunctor ListT where
  hoist morph = ListT . hoist morph . getListT

-- FIXME lift all mtl classes in separate package

instance Foldable m => Foldable (ListT m) where
  foldMap f ListT {getListT } = foldMap f $ runStreamExcept getListT

instance Traversable m => Traversable (ListT m) where
  traverse f ListT {getListT} = traverse f (runStreamExcept getListT) <&> ListT . CoalgebraicExcept
