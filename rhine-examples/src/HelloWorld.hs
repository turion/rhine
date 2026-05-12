{-# LANGUAGE DataKinds #-}

import FRP.Rhine

main :: IO ()
main = flow $ Present ^>>@ (constMCl (putStrLn "Hello World!") @@ (waitClock :: Millisecond 100))
