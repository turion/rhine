module Data.Automaton.Final where

-- base
import Control.Applicative (Alternative (..))

-- mmorph
import Control.Monad.Morph (MFunctor (..))

-- rhine
import Data.Automaton (AutomatonT (..), stepAutomaton)
import Data.Automaton.Result

newtype Final m a = Final {getFinal :: m (Result (Final m a) a)}

toFinal :: (Functor m) => AutomatonT m a -> Final m a
toFinal automaton = Final $ mapResultState toFinal <$> stepAutomaton automaton

fromFinal :: Final m a -> AutomatonT m a
fromFinal final =
  AutomatonT
    { state = final
    , step = getFinal
    }

instance MFunctor Final where
  hoist morph = go
    where
      go Final {getFinal} = Final $ morph $ mapResultState go <$> getFinal

instance (Functor m) => Functor (Final m) where
  fmap f Final {getFinal} = Final $ fmap f . mapResultState (fmap f) <$> getFinal

instance (Applicative m) => Applicative (Final m) where
  pure a = go
    where
      go = Final $! pure $! Result go a

  Final mf <*> Final ma = Final $! (\(Result cf f) (Result ca a) -> Result (cf <*> ca) $! f a) <$> mf <*> ma

constM :: (Functor m) => m a -> Final m a
constM ma = go
  where
    go = Final $ Result go <$> ma

instance (Alternative m) => Alternative (Final m) where
  empty = constM empty

  Final ma1 <|> Final ma2 = Final $ ma1 <|> ma2
