{-# LANGUAGE ImportQualifiedPost #-}

module FRP.Rhine.Clock.Except where

-- base
import Control.Arrow
import Control.Exception
import Control.Exception qualified as Exception
import Control.Monad ((<=<))
import Data.Functor ((<&>))
import Data.Void

-- transformers
import Control.Monad.Trans.MSF.Except

-- time
import Data.Time (UTCTime, getCurrentTime)

-- mtl
import Control.Monad.Error.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.MSF qualified as MSFExcept

-- dunai
import Data.MonadicStreamFunction (morphS)

-- rhine

import Control.Monad.Trans.MSF.Reader (readerS, runReaderS)
import FRP.Rhine.ClSF.Core (ClSF)
import FRP.Rhine.Clock (
  Clock (..),
  HoistClock (..),
  TimeDomain,
  TimeInfo (..),
  retag,
 )
import FRP.Rhine.Clock.Proxy (GetClockProxy)

{- | Handle 'IO' exceptions purely in 'ExceptT'.

The clock @cl@ may throw 'Exception's of type @e@ while running.
These exceptions are automatically caught, and raised as an error in 'ExceptT'
(or more generally in 'MonadError', which implies the presence of 'ExceptT' in the monad transformer stack)

It can then be caught and handled with 'CatchClock'.
-}
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

{- | Catch an exception in one clock and proceed with another.

When @cl@ throws an exception @e@ (in @'ExceptT' e@) while running,
this exception is caught, and a clock @cl'@ is started from the exception value.

For this to be possible, @cl@ must run in the monad @'ExceptT' e m@, while @cl'@ must run in @m@.
To give @cl'@ the ability to throw another exception, you need to add a further 'ExceptT' layer to the stack in @m@.
-}
data CatchClock cl e cl' = CatchClock cl (e -> cl')

instance (Time cl ~ Time cl', Clock (ExceptT e m) cl, Clock m cl', Monad m) => Clock m (CatchClock cl e cl') where
  type Time (CatchClock cl e cl') = Time cl
  type Tag (CatchClock cl e cl') = Either (Tag cl') (Tag cl)
  initClock (CatchClock cl handler) = do
    tryToInit <- runExceptT $ first (>>> arr (second Right)) <$> initClock cl
    -- FIXME Each of these branches needs a unit test
    case tryToInit of
      Right (runningClock, initTime) -> do
        let catchingClock = safely $ do
              e <- MSFExcept.try runningClock
              let cl' = handler e
              (runningClock', _) <- once_ $ initClock cl'
              safe $ runningClock' >>> arr (second Left)
        return (catchingClock, initTime)
      Left e -> (fmap (first (>>> arr (second Left))) . initClock) $ handler e

instance (GetClockProxy (CatchClock cl e cl'))

-- FIXME cl1 cl2 convention everywhere?
catchClSF :: (Time cl1 ~ Time cl2, Monad m) => ClSF m cl1 a b -> ClSF m cl2 a b -> ClSF m (CatchClock cl1 e cl2) a b
catchClSF clsf1 clsf2 = readerS $ proc (timeInfo, a) -> do
  case tag timeInfo of
    Right tag1 -> runReaderS clsf1 -< (retag (const tag1) timeInfo, a)
    Left tag2 -> runReaderS clsf2 -< (retag (const tag2) timeInfo, a)

-- | A clock that throws no exceptions.
type SafeClock m = HoistClock (ExceptT Void m) m

-- | Hoist the monad of a clock into 'ExceptT', without throwing an exception.
safeClock :: (Functor m) => cl -> SafeClock m cl
safeClock unhoistedClock =
  HoistClock
    { unhoistedClock
    , monadMorphism = fmap (either absurd id) . runExceptT
    }

{- | A clock that emits a single tick, and then throws an exception.

The tag, time measurement and exception have to be supplied as clock value.
-}
data Single m time tag e = Single
  { singleTag :: tag
  -- ^ The tag that will be emitted on the tick.
  , getTime :: m time
  -- ^ A method to measure the current time.
  , exception :: e
  -- ^ The exception to throw after the single tick.
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

{- | Catch an exception in clock @cl@ and throw it after one time step.

This is particularly useful if you want to give your signal network a chance to save its current state in some way.
-}
type DelayException m time cl e e' = CatchClock cl e (Single m time e e')

-- | Construct a 'DelayException' clock.
delayException ::
  (Monad m, Clock (ExceptT e m) cl, MonadError e' m) =>
  -- | The clock that will throw an exception @e@
  cl ->
  -- | How to transform the exception into the new exception that will be thrown later
  (e -> e') ->
  -- | How to measure the current time
  m (Time cl) ->
  DelayException m (Time cl) cl e e'
delayException cl handler mTime = CatchClock cl $ \e -> Single e mTime $ handler e

-- | Like 'delayException', but the exception thrown by @cl@ and by the @DelayException@ clock are the same.
delayException' :: (Monad m, MonadError e m, Clock (ExceptT e m) cl) => cl -> m (Time cl) -> DelayException m (Time cl) cl e e
delayException' cl = delayException cl id

-- | Catch an 'IO' 'Exception', and throw it after one time step.
type DelayMonadIOException m cl e e' = DelayException m UTCTime (ExceptClock cl e) e e'

-- | Build a 'DelayMonadIOException'. The time will be measured using the system time.
delayMonadIOException :: (Exception e, MonadIO m, MonadError e' m, Clock IO cl, Time cl ~ UTCTime) => cl -> (e -> e') -> DelayMonadIOException m cl e e'
delayMonadIOException cl handler = delayException (ExceptClock cl) handler $ liftIO getCurrentTime

-- | 'DelayMonadIOException' specialised to 'IOError'.
type DelayMonadIOError m cl e = DelayMonadIOException m cl IOError e

-- | 'delayMonadIOException' specialised to 'IOError'.
delayMonadIOError :: (Exception e, MonadError e m, MonadIO m, Clock IO cl, Time cl ~ UTCTime) => cl -> (IOError -> e) -> DelayMonadIOError m cl e
delayMonadIOError = delayMonadIOException

-- | Like 'delayMonadIOError', but throw the error without transforming it.
delayMonadIOError' :: (MonadError IOError m, MonadIO m, Clock IO cl, Time cl ~ UTCTime) => cl -> DelayMonadIOError m cl IOError
delayMonadIOError' cl = delayMonadIOError cl id

{- | 'DelayMonadIOException' specialised to the monad @'ExceptT' e' 'IO'@.

This is sometimes helpful when the type checker complains about an ambigous monad type variable.
-}
type DelayIOException cl e e' = DelayException (ExceptT e' IO) UTCTime (ExceptClock cl e) e e'

-- | 'delayMonadIOException' specialised to the monad @'ExceptT' e' 'IO'@.
delayIOException :: (Exception e, Clock IO cl, Time cl ~ UTCTime) => cl -> (e -> e') -> DelayIOException cl e e'
delayIOException = delayMonadIOException

-- | 'delayIOException'', but throw the error without transforming it.
delayIOException' :: (Exception e, Clock IO cl, Time cl ~ UTCTime) => cl -> DelayIOException cl e e
delayIOException' cl = delayIOException cl id

-- | 'DelayIOException' specialised to 'IOError'.
type DelayIOError cl e = DelayIOException cl IOError e

-- | 'delayIOException' specialised to 'IOError'.
delayIOError :: (Time cl ~ UTCTime, Clock IO cl) => cl -> (IOError -> e) -> DelayIOError cl e
delayIOError = delayIOException

-- | 'delayIOError', but throw the error without transforming it.
delayIOError' :: (Time cl ~ UTCTime, Clock IO cl) => cl -> DelayIOError cl IOError
delayIOError' cl = delayIOException cl id
