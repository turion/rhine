-- base
import Control.Monad (guard)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, listToMaybe)
import Text.Printf

-- random
import System.Random

-- rhine
import FRP.Rhine

data Ball = Ball Double Double Double
  deriving (Eq, Show)

type BallVel = Ball

instance VectorSpace Ball (Seconds Double) where
  zeroVector = Ball 0 0 0
  (Ball x1 y1 z1) ^+^ (Ball x2 y2 z2) = Ball (x1 + x2) (y1 + y2) (z1 + z2)
  Seconds r *^ (Ball x y z) = Ball (r * x) (r * y) (r * z)
  dot (Ball x1 y1 z1) (Ball x2 y2 z2) = Seconds $ x1 * x2 + y1 * y2 + z1 * z2

type SimClock = Millisecond 10
type StatusClock = Millisecond 500

freeFall ::
  (Monad m) =>
  BallVel ->
  BehaviourF m UTCTime () Ball
freeFall v0 =
  arr (const (Ball 0 0 (-9.81)))
    >>> integralFrom v0
    >>> integral

startVel :: ClSF IO StdinClock () BallVel
startVel = arrMCl $ const $ do
  velX <- randomRIO (-10, 10)
  velY <- randomRIO (-10, 10)
  velZ <- randomRIO (3, 10)
  return $ Ball velX velY velZ

waiting ::
  (Monad m) =>
  ClSF
    (ExceptT BallVel m)
    SimClock
    (Maybe BallVel)
    Ball
waiting = throwMaybe >>> arr (const zeroVector)

falling ::
  (Monad m) =>
  BallVel ->
  ClSF
    (ExceptT () m)
    SimClock
    (Maybe BallVel)
    Ball
falling v0 = proc _ -> do
  pos <- freeFall v0 -< ()
  let Ball _ _ height = pos
  throwMaybe -< guard $ height < 0
  returnA -< pos

ballModes :: ClSFExcept SimClock (Maybe BallVel) Ball IO void
ballModes = do
  v0 <- try waiting
  once_ $ putStrLn "Catch!"
  try $ falling v0
  once_ $ putStrLn "Caught!"
  ballModes

ball :: ClSF IO SimClock (Maybe BallVel) Ball
ball = safely ballModes

downsampleSimToStatus :: ResBuf IO SimClock StatusClock Ball Ball
downsampleSimToStatus = collect <&> (listToMaybe >>> fromMaybe zeroVector)

statusMsg :: ClSF IO StatusClock Ball ()
statusMsg = arrMCl $ \(Ball x y z) ->
  printf "%.2f %.2f %.2f\n" x y z

startVelRh :: Rhine IO StdinClock () BallVel
startVelRh = startVel @@ StdinClock

ballRh :: Rhine IO SimClock (Maybe BallVel) Ball
ballRh = ball @@ waitClock

statusRh :: Rhine IO StatusClock Ball ()
statusRh = statusMsg @@ waitClock

ballStatusRh :: Rhine IO (SeqClock SimClock StatusClock) (Maybe BallVel) ()
ballStatusRh = ballRh >-- downsampleSimToStatus --> statusRh

main :: IO ()
main =
  flow $
    startVelRh
      >-- fifoUnbounded
      --> ballStatusRh
