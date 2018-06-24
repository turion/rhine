{-# LANGUAGE DataKinds #-}
import FRP.Rhine
import FRP.Rhine.Clock.Realtime.Millisecond

main :: IO ()
main = flow $ constMCl (putStrLn "Hello World!") @@ (waitClock :: Millisecond 100)
