{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Several utilities to create 'ResamplingBuffer's.
-}
module FRP.Rhine.ResamplingBuffer.Util where

-- transformers
import Control.Monad.Trans.Reader (runReaderT)

-- dunai
import Data.MonadicStreamFunction.InternalCore

-- rhine

import FRP.Rhine.ClSF
import FRP.Rhine.Clock
import FRP.Rhine.ResamplingBuffer

-- * Utilities to build 'ResamplingBuffer's from smaller components

{- FOURMOLU_DISABLE -}
{-

infix 2 >>-^

-- | Postcompose a 'ResamplingBuffer' with a matching 'ClSF'.
(>>-^) ::
  Monad m =>
  ResamplingBuffer m cl1 cl2 a b   ->
  ClSF             m     cl2   b c ->
  ResamplingBuffer m cl1 cl2 a   c
resBuf >>-^ clsf = ResamplingBuffer put_ get_
  where
    put_ theTimeInfo a = (>>-^ clsf) <$> put resBuf theTimeInfo a
    get_ theTimeInfo = do
      (b, resBuf') <- get resBuf theTimeInfo
      (c, clsf') <- unMSF clsf b `runReaderT` theTimeInfo
      return (c, resBuf' >>-^ clsf')

infix 1 ^->>

-- | Precompose a 'ResamplingBuffer' with a matching 'ClSF'.
(^->>) ::
  Monad m =>
  ClSF             m cl1     a b   ->
  ResamplingBuffer m cl1 cl2   b c ->
  ResamplingBuffer m cl1 cl2 a   c
clsf ^->> resBuf = ResamplingBuffer put_ get_
  where
    put_ theTimeInfo a = do
      (b, clsf') <- unMSF clsf a `runReaderT` theTimeInfo
      resBuf' <- put resBuf theTimeInfo b
      return $ clsf' ^->> resBuf'
    get_ theTimeInfo   = second (clsf ^->>) <$> get resBuf theTimeInfo
-}

infixl 4 *-*

-- | Parallely compose two 'ResamplingBuffer's.
(*-*) ::
  Monad m =>
  ResamplingBuffer m cl1 cl2 a b ->
  ResamplingBuffer m cl1 cl2 c d ->
  ResamplingBuffer m cl1 cl2 (a, c) (b, d)
(ResamplingBuffer putL getL sL0) *-* (ResamplingBuffer putR getR sR0) = ResamplingBuffer {..}
  where
    resamplingState = (sL0, sR0)
    put (sL, sR) theTimeInfo (a, c) = do
      sL' <- putL sL theTimeInfo a
      sR' <- putR sR theTimeInfo c
      return (sL', sR')
    get (sL, sR) theTimeInfo = do
      (b, sL') <- getL sL theTimeInfo
      (d, sR') <- getR sR theTimeInfo
      return ((b, d), (sL', sR'))

{-
infixl 4 &-&

-- | Parallely compose two 'ResamplingBuffer's, duplicating the input.
(&-&) ::
  Monad m =>
  ResamplingBuffer m cl1 cl2  a  b    ->
  ResamplingBuffer m cl1 cl2  a     c ->
  ResamplingBuffer m cl1 cl2  a (b, c)
resBuf1 &-& resBuf2 = arr (\a -> (a, a)) ^->> resBuf1 *-* resBuf2

-- | Given a 'ResamplingBuffer' where the output type depends on the input type polymorphically,
--   we can produce a timestamped version that simply annotates every input value
--   with the 'TimeInfo' when it arrived.
timestamped
  :: Monad m
  => (forall b. ResamplingBuffer m cl clf b (f b))
  -> ResamplingBuffer m cl clf a (f (a, TimeInfo cl))
timestamped resBuf = (clId &&& timeInfo) ^->> resBuf
-}
