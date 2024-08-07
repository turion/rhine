{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Automaton.Recursive where

-- base
import Control.Applicative (Alternative)
import Control.Arrow
import Control.Category
import Prelude hiding (id, (.))

-- transformers
import Control.Monad.Trans.Reader

-- automaton
import Data.Automaton
import Data.Stream.Optimized qualified as StreamOptimized
import Data.Stream.Recursive qualified as StreamRecursive

{- | Automata in direct recursive encoding.

This type is isomorphic to @MSF@ from @dunai@.
-}
newtype Recursive m a b = Recursive {getRecursive :: StreamRecursive.Recursive (ReaderT a m) b}
  deriving newtype (Functor, Applicative, Alternative)

instance (Monad m) => Category (Recursive m) where
  id = toRecursive id
  f1 . f2 = toRecursive $ fromRecursive f1 . fromRecursive f2

instance (Monad m) => Arrow (Recursive m) where
  arr = toRecursive . arr
  first = toRecursive . first . fromRecursive

toRecursive :: (Functor m) => Automaton m a b -> Recursive m a b
toRecursive (Automaton automaton) = Recursive $ StreamOptimized.toRecursive automaton

fromRecursive :: Recursive m a b -> Automaton m a b
fromRecursive Recursive {getRecursive} = Automaton $ StreamOptimized.fromRecursive getRecursive
