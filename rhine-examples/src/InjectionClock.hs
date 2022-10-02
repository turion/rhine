{-# LANGUAGE DataKinds #-}

-- rhine
import FRP.Rhine


type Second5 = Millisecond 5000
type Second = Millisecond 1000

-- | On every newline, toggle the velocity between 1 and -1.
setVel :: ClSF IO StdinClock () Integer
setVel = feedback (1 :: Integer) $ arr $ \ ((), old) ->
  if old == 1
  then (-1, -1)
  else (1, 1)

-- | Every second, updat the position based on the incoming velocity.
setPos :: ClSF IO Second ((), Integer) Integer
setPos = feedback 0 $ arr $ \(((), v), old) -> (old + v, old + v)

-- | Inject the velocity into 'setPos'.
updatePos :: Rhine IO (InjectionClock IO StdinClock Second) () Integer
updatePos = setVel @@ StdinClock >=- keepLast 1 -@- concurrently -=> setPos @@ waitClock

-- | Print a string if there is one, and an integer otherwise.
printFunc :: (Integer, Maybe String) -> IO ()
printFunc (x, mString) = case mString of
                           Just str -> putStrLn str
                           Nothing  -> print x

-- | Inject the string "Hello" every 5 seconds. Otherwise print the incoming integer.
printInfo :: Rhine IO (InjectionClock IO Second5 Second) Integer ()
printInfo = arr (const "Hello") @@ waitClock >=- fifoUnbounded -@- concurrently -=> arrMCl printFunc @@ waitClock

-- | Sequential composition of the two injections.
--   Every second, update the position based on injected velocity
--   and either print the position or the string.
printPos :: Rhine IO (InjectionClock IO (ParallelClock IO StdinClock Second5) Second) () ()
printPos = updatePos >>=- concurrently =@= concurrently -=>> printInfo

main :: IO ()
main = flow printPos
