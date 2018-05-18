-- base
import Data.List

-- rhine
import FRP.Rhine
import FRP.Rhine.Clock.Realtime.Busy
import FRP.Rhine.Clock.Realtime.Stdin
import FRP.Rhine.ResamplingBuffer.Collect
import FRP.Rhine.ResamplingBuffer.Timeless
import FRP.Rhine.ResamplingBuffer.Util
import FRP.Rhine.Schedule.Concurrently

rb :: (Monad m, Num a) => ResamplingBuffer m cla clb a a
rb = timelessResamplingBuffer AsyncMealy
  { amPut = \a s -> let s' = a + s in s' `seq` return $ s'
  , amGet = \  s -> return (s, s)
  } 0

rb' :: (Monad m, Num a) => ResamplingBuffer m cla clb a a
rb' = go 0
  where
    go s = ResamplingBuffer
      { put = \_ a -> return $ go $ a + s
      , get = \_   -> return (s, go s)
    }

rb'' :: (Monad m, Num a) => ResamplingBuffer m cla clb a a
rb'' = pureBuffer $ foldl' (+) 0

rb''' :: (Monad m, Num a) => ResamplingBuffer m cla clb a a
rb''' = collect >>-^ arr (foldl' (+) 0)

thing1 = timeInfoOf sinceStart >>> arr sin

main = do
  putStrLn "Press return to force the buffer"
  flow $ thing1 @@ Busy >-- rb -@- concurrently --> arrMSync print @@ StdinClock
