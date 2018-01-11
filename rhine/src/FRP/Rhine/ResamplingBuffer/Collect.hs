{- | Resampling buffers that collect the incoming data in some data structure
and release all of it on output.
-}
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
module FRP.Rhine.ResamplingBuffer.Collect where

-- containers
import Data.Sequence

-- rhine
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.ResamplingBuffer.Timeless

-- | Collects all input in a list, with the newest element at the head,
--   which is returned and emptied upon `get`.
collect :: Monad m => ResamplingBuffer m cl1 cl2 a [a]
collect = timelessResamplingBuffer AsyncMealy {..} []
  where
    amPut as a = return $ a : as
    amGet as   = return (as, [])


-- | Reimplementation of 'collect' with sequences,
--   which gives a performance benefit if the sequence needs to be reversed or searched.
collectSequence :: Monad m => ResamplingBuffer m cl1 cl2 a (Seq a)
collectSequence = timelessResamplingBuffer AsyncMealy {..} empty
  where
    amPut as a = return $ a <| as
    amGet as   = return (as, empty)

-- | 'pureBuffer' collects all input values lazily in a list
--   and processes it when output is required.
--   Semantically, @pureBuffer f == collect >>-^ arr f@,
--   but 'pureBuffer' is slightly more efficient.
pureBuffer :: Monad m => ([a] -> b) -> ResamplingBuffer m cl1 cl2 a b
pureBuffer f = timelessResamplingBuffer AsyncMealy {..} []
  where
    amPut as a = return (a : as)
    amGet as   = return (f as, [])

-- TODO Test whether strictness works here, or consider using deepSeq
-- | A buffer collecting all incoming values with a folding function.
--   It is strict, i.e. the state value 'b' is calculated on every 'put'.
foldBuffer
  :: Monad m
  => (a -> b -> b) -- ^ The folding function
  -> b -- ^ The initial value
  -> ResamplingBuffer m cl1 cl2 a b
foldBuffer f = timelessResamplingBuffer AsyncMealy {..}
  where
    amPut b a = let !b' = f a b in return b'
    amGet b   = return (b, b)
