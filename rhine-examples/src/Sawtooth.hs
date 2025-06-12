{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Generate a simple sawtooth signal using exceptions and 'sinceStart'.
-}
module Main where

import FRP.Rhine

sawtooth :: (Monad m) => Behaviour m UTCTime Double
sawtooth = safely $ do
  try $
    sinceStart >>> proc time -> do
      throwOn () -< time > 1
      returnA -< time
  safe sawtooth

main :: IO ()
main = flow $ sawtooth >-> arrMCl print @@ (waitClock :: Millisecond 200)
