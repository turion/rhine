-- base
import Control.Monad (guard)
import Text.Printf

-- random
import System.Random

-- vector-sized
import Data.Vector.Sized as VS

-- rhine
import FRP.Rhine

type Ball = (Double, Double, Double)
type BallVel = (Double, Double, Double)

type SimClock = Millisecond 10
type StatusClock = Millisecond 500

freeFall ::
  Monad m =>
  BallVel ->
  BehaviourF m UTCTime () Ball
freeFall v0 =
  arr (const (0, 0, -9.81))
    >>> integralFrom v0
    >>> integral

startVel :: ClSF IO StdinClock () BallVel
startVel = arrMCl $ const $ do
  velX <- randomRIO (-10, 10)
  velY <- randomRIO (-10, 10)
  velZ <- randomRIO (3, 10)
  return (velX, velY, velZ)

waiting ::
  MonadIO m =>
  ClSF
    (ExceptT BallVel m)
    SimClock
    (Maybe BallVel)
    Ball
waiting = throwMaybe >>> arr (const zeroVector)

falling ::
  Monad m =>
  BallVel ->
  ClSF
    (ExceptT () m)
    SimClock
    (Maybe BallVel)
    Ball
falling v0 = proc _ -> do
  pos <- freeFall v0 -< ()
  let (_, _, height) = pos
  throwMaybe -< guard $ height < 0
  returnA -< pos

ballModes :: ClSFExcept IO SimClock (Maybe BallVel) Ball void
ballModes = do
  v0 <- try waiting
  once_ $ putStrLn "Catch!"
  try $ falling v0
  once_ $ putStrLn "Caught!"
  ballModes

ball :: ClSF IO SimClock (Maybe BallVel) Ball
ball = safely ballModes

downsampleSimToStatus :: ResBuf IO SimClock StatusClock Ball Ball
downsampleSimToStatus =
  downsampleMillisecond
    >>-^ arr VS.head

statusMsg :: ClSF IO StatusClock Ball ()
statusMsg = arrMCl $ \(x, y, z) ->
  printf "%.2f %.2f %.2f\n" x y z

startVelRh :: Rhine IO StdinClock () BallVel
startVelRh = startVel @@ StdinClock

ballRh :: Rhine IO SimClock (Maybe BallVel) Ball
ballRh = ball @@ waitClock

statusRh :: Rhine IO StatusClock Ball ()
statusRh = statusMsg @@ waitClock

simToStatus :: ResamplingPoint IO SimClock StatusClock Ball Ball
simToStatus = downsampleSimToStatus -@- scheduleMillisecond

ballStatusRh :: Rhine IO (SeqClock IO SimClock StatusClock) (Maybe BallVel) ()
ballStatusRh = ballRh >-- simToStatus --> statusRh

main :: IO ()
main =
  flow $
    startVelRh
      >-- fifoUnbounded -@- concurrently
      --> ballStatusRh
