{-# LANGUAGE DataKinds #-}
import FRP.Rhine
import FRP.Rhine.Clock.Realtime.Millisecond

main :: IO ()
main = flow $ arrMSync_ (putStrLn "Hello World!") @@ (waitClock :: Millisecond 100)
