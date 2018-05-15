import FRP.Rhine
import FRP.Rhine.Clock.Realtime.Busy
import FRP.Rhine.Clock.Realtime.Stdin
import FRP.Rhine.ResamplingBuffer.Timeless
import FRP.Rhine.Schedule.Concurrently

rb :: (Monad m, Num a) => ResamplingBuffer m cla clb a a
rb = timelessResamplingBuffer AsyncMealy
  { amPut = \a s -> let s' = a + s in s' `seq` return $ s'
  , amGet = \  s -> return (s, s)
  } 0

rb' = go 0
  where
    go s = ResamplingBuffer
      { put = \_ a -> return $ go $ a + s
      , get = \_   -> return (s, go s)
    }

thing1 = timeInfoOf sinceStart >>> arr sin

main = do
  putStrLn "Press return to force the buffer"
  flow $ thing1 @@ Busy >-- rb' -@- concurrently --> arrMSync print @@ StdinClock
