{-# LANGUAGE ImportQualifiedPost #-}

module FRP.Rhine.Clock.Except where

import Control.Exception
import Control.Exception qualified as Exception
import Control.Monad ((<=<), (>=>))
import Control.Monad.Error.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.MSF qualified as MSFExcept
import Control.Monad.Trans.MSF.Except
import Data.Functor ((<&>))
import Data.Time (getCurrentTime)
import Data.Void
import FRP.Rhine (GetClockProxy)
import FRP.Rhine.Clock

newtype ExceptClock cl e = ExceptClock {getExceptClock :: cl}

instance (Exception e, Clock IO cl, MonadIO eio, MonadError e eio) => Clock eio (ExceptClock cl e) where
  type Time (ExceptClock cl e) = Time cl
  type Tag (ExceptClock cl e) = Tag cl

  initClock ExceptClock {getExceptClock} = do
    ioerror $
      Exception.try $
        initClock getExceptClock
          <&> first (morphS (ioerror . Exception.try))
    where
      ioerror :: (MonadError e eio, MonadIO eio) => IO (Either e a) -> eio a
      ioerror = liftEither <=< liftIO

instance GetClockProxy (ExceptClock cl e)

data CatchClock cl e cl' = CatchClock cl (e -> cl')

instance (Time cl ~ Time cl', Clock (ExceptT e m) cl, Clock m cl', Monad m) => Clock m (CatchClock cl e cl') where
  type Time (CatchClock cl e cl') = Time cl
  type Tag (CatchClock cl e cl') = Either (Tag cl) (Tag cl')
  initClock (CatchClock cl handler) = do
    tryToInit <- runExceptT $ first (>>> arr (second Left)) <$> initClock cl
    -- FIXME Each of these branches needs a unit test
    case tryToInit of
      Right (runningClock, initTime) -> do
        let catchingClock = safely $ do
              e <- MSFExcept.try runningClock
              let cl' = handler e
              (runningClock', _) <- once_ $ initClock cl'
              safe $ runningClock' >>> arr (second Right)
        return (catchingClock, initTime)
      Left e -> (fmap (first (>>> arr (second Right))) . initClock) $ handler e

instance (GetClockProxy (CatchClock cl e cl'))

type SafeClock m = HoistClock (ExceptT Void m) m

safeClock :: (Functor m) => cl -> SafeClock m cl
safeClock unhoistedClock =
  HoistClock
    { unhoistedClock
    , monadMorphism = fmap (either absurd id) . runExceptT
    }

data Single m time tag e = Single
  { singleTag :: tag
  , getTime :: m time
  , exception :: e
  }

instance (TimeDomain time, MonadError e m) => Clock m (Single m time tag e) where
  type Time (Single m time tag e) = time
  type Tag (Single m time tag e) = tag
  initClock Single {singleTag, getTime, exception} = do
    initTime <- getTime
    let runningClock = morphS (errorT . runExceptT) $ runMSFExcept $ do
          step_ (initTime, singleTag)
          return exception
        errorT :: (MonadError e m) => m (Either e a) -> m a
        errorT = (>>= liftEither)
    return (runningClock, initTime)

type DelayException m time cl e e' = CatchClock cl e (Single m time e e')

delayException :: (Monad m, Clock (ExceptT e m) cl, MonadError e' m) => cl -> (e -> e') -> m (Time cl) -> DelayException m (Time cl) cl e e'
delayException cl handler mTime = CatchClock cl $ \e -> Single e mTime $ handler e

delayException' :: (Monad m, MonadError e m, Clock (ExceptT e m) cl) => cl -> m (Time cl) -> DelayException m (Time cl) cl e e
delayException' cl = delayException cl id

type DelayMonadIOException m cl e e' = DelayException m UTCTime (ExceptClock cl e) e e'

delayMonadIOException :: (Exception e, MonadIO m, MonadError e' m, Clock IO cl, Time cl ~ UTCTime) => cl -> (e -> e') -> DelayMonadIOException m cl e e'
delayMonadIOException cl handler = delayException (ExceptClock cl) handler $ liftIO getCurrentTime

type DelayMonadIOError m cl e = DelayMonadIOException m cl IOError e

delayMonadIOError :: (Exception e, MonadError e m, MonadIO m, Clock IO cl, Time cl ~ UTCTime) => cl -> (IOError -> e) -> DelayMonadIOError m cl e
delayMonadIOError = delayMonadIOException

delayMonadIOError' :: (MonadError IOError m, MonadIO m, Clock IO cl, Time cl ~ UTCTime) => cl -> DelayMonadIOError m cl IOError
delayMonadIOError' cl = delayMonadIOError cl id

type DelayIOException cl e e' = DelayException (ExceptT e' IO) UTCTime (ExceptClock cl e) e e'

delayIOException :: (Exception e, Clock IO cl, Time cl ~ UTCTime) => cl -> (e -> e') -> DelayIOException cl e e'
delayIOException cl handler = delayException (ExceptClock cl) handler $ liftIO getCurrentTime

delayIOException' :: (Exception e, Clock IO cl, Time cl ~ UTCTime) => cl -> DelayIOException cl e e
delayIOException' cl = delayIOException cl id

type DelayIOError cl e = DelayIOException cl IOError e

delayIOError :: (Time cl ~ UTCTime, Clock IO cl) => cl -> (IOError -> e) -> DelayIOError cl e
delayIOError = delayIOException

delayIOError' :: (Time cl ~ UTCTime, Clock IO cl) => cl -> DelayIOError cl IOError
delayIOError' cl = delayIOException cl id
