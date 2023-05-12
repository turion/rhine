{-# LANGUAGE DataKinds #-}

import FRP.Rhine

main :: IO ()
main = flow $ constMCl (putStrLn "Hello World!") @@ (waitClock :: Millisecond 100)
