{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

-- | Helper functions and types for Data.Stream. You will typically not need them.
module Data.Stream.Internal where

-- | A strict tuple type
data JointState a b = JointState a b

-- | Internal state of the result of 'Alternative' constructions
data Alternatively stateL stateR = Undecided | DecideL stateL | DecideR stateR

-- | Internal state of 'many' and 'some'
data Many state x = NotStarted | Ongoing x state | Finished

-- newtype makes GHC loop on using fixStream
{- HLINT ignore Fix "Use newtype instead of data" -}
data Fix t = Fix {getFix :: ~(t (Fix t))}

fixState :: (forall s. s -> t s) -> Fix t
fixState transformState = go
  where
    go = Fix $ transformState go
