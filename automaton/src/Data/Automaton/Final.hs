{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Automaton.Final where

-- base
import Control.Applicative (Alternative)
import Control.Arrow
import Control.Category
import Prelude hiding (id, (.))

-- transformers
import Control.Monad.Trans.Reader

-- automaton
import Data.Automaton
import Data.Stream.Final qualified as StreamFinal
import Data.Stream.Optimized qualified as StreamOptimized

-- | Automata in final encoding.
newtype Final m a b = Final {getFinal :: StreamFinal.Final (ReaderT a m) b}
  deriving newtype (Functor, Applicative, Alternative)

instance (Monad m) => Category (Final m) where
  id = toFinal id
  f1 . f2 = toFinal $ fromFinal f1 . fromFinal f2

instance (Monad m) => Arrow (Final m) where
  arr = toFinal . arr
  first = toFinal . first . fromFinal

toFinal :: (Functor m) => Automaton m a b -> Final m a b
toFinal (Automaton automaton) = Final $ StreamOptimized.toFinal automaton

fromFinal :: Final m a b -> Automaton m a b
fromFinal Final {getFinal} = Automaton $ StreamOptimized.fromFinal getFinal
