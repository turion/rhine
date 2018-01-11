-- | Several utilities to create 'ResamplingBuffer's.

{-# LANGUAGE RankNTypes #-}
module FRP.Rhine.ResamplingBuffer.Util where

-- transformers
import Control.Monad.Trans.Reader (runReaderT)

-- rhine
import FRP.Rhine

-- * Utilities to build 'ResamplingBuffer's from smaller components

infix 2 >>-^
-- | Postcompose a 'ResamplingBuffer' with a matching 'SyncSF'.
(>>-^) :: Monad m
      => ResamplingBuffer m cl1 cl2 a b
      -> SyncSF           m     cl2   b c
      -> ResamplingBuffer m cl1 cl2 a   c
resBuf >>-^ syncSF = ResamplingBuffer put_ get_
  where
    put_ theTimeInfo a = (>>-^ syncSF) <$> put resBuf theTimeInfo a
    get_ theTimeInfo   = do
      (b, resBuf') <- get resBuf theTimeInfo
      (c, syncSF') <- unMSF syncSF b `runReaderT` theTimeInfo
      return (c, resBuf' >>-^ syncSF')


infix 1 ^->>
-- | Precompose a 'ResamplingBuffer' with a matching 'SyncSF'.
(^->>) :: Monad m
      => SyncSF           m cl1     a b
      -> ResamplingBuffer m cl1 cl2   b c
      -> ResamplingBuffer m cl1 cl2 a   c
syncSF ^->> resBuf = ResamplingBuffer put_ get_
  where
    put_ theTimeInfo a = do
      (b, syncSF') <- unMSF syncSF a `runReaderT` theTimeInfo
      resBuf'      <- put resBuf theTimeInfo b
      return $ syncSF' ^->> resBuf'
    get_ theTimeInfo   = second (syncSF ^->>) <$> get resBuf theTimeInfo


infix 4 *-*
-- | Parallely compose two 'ResamplingBuffer's.
(*-*) :: Monad m
      => ResamplingBuffer m cl1 cl2  a      b
      -> ResamplingBuffer m cl1 cl2     c      d
      -> ResamplingBuffer m cl1 cl2 (a, c) (b, d)
resBuf1 *-* resBuf2 = ResamplingBuffer put_ get_
  where
    put_ theTimeInfo (a, c) = do
      resBuf1' <- put resBuf1 theTimeInfo a
      resBuf2' <- put resBuf2 theTimeInfo c
      return $ resBuf1' *-* resBuf2'
    get_ theTimeInfo        = do
      (b, resBuf1') <- get resBuf1 theTimeInfo
      (d, resBuf2') <- get resBuf2 theTimeInfo
      return ((b, d), resBuf1' *-* resBuf2')

-- | Given a 'ResamplingBuffer' where the output type depends on the input type polymorphically,
--   we can produce a timestamped version that simply annotates every input value
--   with the 'TimeInfo' when it arrived.
timestamped
  :: Monad m
  => (forall b. ResamplingBuffer m cl clf b (f b))
  -> ResamplingBuffer m cl clf a (f (a, TimeInfo cl))
timestamped resBuf = (syncId &&& timeInfo) ^->> resBuf
