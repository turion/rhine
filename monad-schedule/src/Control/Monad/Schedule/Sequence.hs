{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.Schedule.Sequence where

-- base
import Control.Arrow ((>>>))
import Control.Monad.IO.Class
import Data.Functor.Identity

-- transformers
import Control.Monad.Trans.Class

-- monad-schedule
import Control.Monad.Schedule.Class

-- | Any monad can be trivially scheduled by executing all actions sequentially.
newtype SequenceT m a = SequenceT {unSequence :: m a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans SequenceT where
  lift = SequenceT

{- | Execute all actions in sequence and return their result when all of them are done.
  Essentially, this is 'sequenceA'.
-}
instance (Monad m) => MonadSchedule (SequenceT m) where
  schedule = sequenceA >>> fmap (,[])

type Sequence = SequenceT Identity
