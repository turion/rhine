-- | If a clock in 'IO' throws an 'IOException', you can wrap it in 'Catch' to stop on it.
module FRP.Rhine.Clock.Catch where

-- base
import Control.Exception (Exception, catchJust, throw, tryJust)
import Control.Monad.IO.Class

-- time
import Data.Time (getCurrentTime)

-- rhine
import Control.Monad.Trans.MSF (safely)
import Control.Monad.Trans.MSF.Except (
  ExceptT (ExceptT),
  once,
  safe,
  step_,
  try,
 )
import FRP.Rhine (GetClockProxy)
import FRP.Rhine.Clock

data Catch cl e cl' = Catch
  { throwing :: cl
  , handler :: e -> Maybe cl'
  }

type CatchIOError cl cl' = Catch cl IOError cl'

instance (Time cl ~ Time cl', Clock IO cl, Clock IO cl', Exception e) => Clock IO (Catch cl e cl') where
  type Time (Catch cl e cl') = Time cl
  type Tag (Catch cl e cl') = Either (Tag cl') (Tag cl)
  initClock Catch {throwing, handler} = do
    (runningClock, initialTime) <-
      catchJust
        handler
        (first (>>> arr (second Right)) <$> initClock throwing)
        (fmap (first (>>> arr (second Left))) . initClock)
    let catchingClock = safely $ do
          cl' <- try $ morphS (ExceptT . tryJust handler) runningClock
          (runningClock', _initialTime) <- once $ const $ initClock cl'
          safe $ runningClock' >>> arr (second Left)
    return (catchingClock, initialTime)

instance GetClockProxy (Catch cl e cl')

-- FIXME naming is inconsistent with MSFExcept, it's more like "Step"
data Once a e = Once a e

instance (MonadIO io, Exception e) => Clock io (Once a e) where
  type Time (Once a e) = UTCTime
  type Tag (Once a e) = a
  initClock (Once a exception) = do
    initialTime <- liftIO getCurrentTime
    let runningClock = safely $ do
          step_ (initialTime, a)
          safe $ constM $ liftIO $ throw exception
    return (runningClock, initialTime)

type CatchOnce cl e = Catch cl e (Once () e)

catchOnce :: cl -> (e -> Bool) -> CatchOnce cl e
catchOnce cl handler =
  Catch
    { throwing = cl
    , handler = \e -> if handler e then Just (Once () e) else Nothing
    }

type CatchOnceIOError cl = CatchOnce cl IOError

catchOnceIOError :: cl -> (IOError -> Bool) -> CatchOnceIOError cl
catchOnceIOError = catchOnce
