{-# LANGUAGE DataKinds #-}

import FRP.Rhine hiding ((^>>>), (@@), flow)
import FRP.Rhine.Rhine.Free
import FRP.Rhine.SN.Free

main :: IO ()
main = flow $ Present ^>>> constMCl (putStrLn "Hello World!") @@ (waitClock :: Millisecond 100)
