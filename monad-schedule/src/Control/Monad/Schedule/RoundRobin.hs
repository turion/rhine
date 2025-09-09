{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.Schedule.RoundRobin where

-- base
import Control.Monad.IO.Class
import Data.Functor.Identity
import qualified Data.List.NonEmpty as NonEmpty

-- transformers
import Control.Monad.Trans.Class

-- monad-schedule
import Control.Monad.Schedule.Class

{- | Any monad can be trivially scheduled by executing all actions after each other,
  step by step.
-}
newtype RoundRobinT m a = RoundRobinT {unRoundRobin :: m a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans RoundRobinT where
  lift = RoundRobinT

-- | Execute only the first action, and leave the others for later, preserving the order.
instance (Monad m) => MonadSchedule (RoundRobinT m) where
  schedule actions = (,NonEmpty.tail actions) <$> fmap pure (NonEmpty.head actions)

type RoundRobin = RoundRobinT Identity
