module FRP.Rhine.Clock.Except where

-- base
import Control.Arrow
import Control.Exception
import Control.Exception qualified as Exception
import Control.Monad ((<=<))
import Data.Void

-- time
import Data.Time (UTCTime, getCurrentTime)

-- mtl
import Control.Monad.Error.Class
import Control.Monad.IO.Class (MonadIO, liftIO)

-- time-domain
import Data.TimeDomain (TimeDomain)

-- automaton
import Data.Automaton (hoistS)
import Data.Automaton.Trans.Except
import Data.Automaton.Trans.Except qualified as AutomatonExcept
import Data.Automaton.Trans.Reader (readerS, runReaderS)

-- rhine
import FRP.Rhine.ClSF.Core (ClSF)
import FRP.Rhine.Clock (
  Clock (..),
  HoistClock (..),
  TimeInfo (..),
  retag,
 )
import FRP.Rhine.Clock.Proxy (GetClockProxy)

-- * 'ExceptClock'

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

  initClock ExceptClock {getExceptClock} = hoistS (ioerror . Exception.try) $ initClock getExceptClock
    where
      ioerror :: (MonadError e eio, MonadIO eio) => IO (Either e a) -> eio a
      ioerror = liftEither <=< liftIO

instance GetClockProxy (ExceptClock cl e)

-- * 'CatchClock'

{- | Catch an exception in one clock and proceed with another.

When @cl1@ throws an exception @e@ (in @'ExceptT' e@) while running,
this exception is caught, and a clock @cl2@ is started from the exception value.

For this to be possible, @cl1@ must run in the monad @'ExceptT' e m@, while @cl2@ must run in @m@.
To give @cl2@ the ability to throw another exception, you need to add a further 'ExceptT' layer to the stack in @m@.
-}
data CatchClock cl1 e cl2 = CatchClock cl1 (e -> cl2)

instance (Time cl1 ~ Time cl2, Clock (ExceptT e m) cl1, Clock m cl2, Monad m) => Clock m (CatchClock cl1 e cl2) where
  type Time (CatchClock cl1 e cl2) = Time cl1
  type Tag (CatchClock cl1 e cl2) = Either (Tag cl2) (Tag cl1)
  initClock (CatchClock cl1 handler) = safely $ do
    e <- AutomatonExcept.try $ initClock cl1 >>> arr (second Right)
    let cl2 = handler e
    safe $ initClock cl2 >>> arr (second Left)

instance (GetClockProxy (CatchClock cl1 e cl2))

-- | Combine two 'ClSF's under two different clocks.
catchClSF ::
  (Time cl1 ~ Time cl2, Monad m) =>
  -- | Executed until @cl1@ throws an exception
  ClSF m cl1 a b ->
  -- | Executed after @cl1@ threw an exception, when @cl2@ is started
  ClSF m cl2 a b ->
  ClSF m (CatchClock cl1 e cl2) a b
catchClSF clsf1 clsf2 = readerS $ proc (timeInfo, a) -> do
  case tag timeInfo of
    Right tag1 -> runReaderS clsf1 -< (retag (const tag1) timeInfo, a)
    Left tag2 -> runReaderS clsf2 -< (retag (const tag2) timeInfo, a)

-- * 'SafeClock'

-- | A clock that throws no exceptions.
type SafeClock m = HoistClock (ExceptT Void m) m

-- | Remove 'ExceptT' from the monad of a clock, proving that no exception can be thrown.
safeClock :: (Functor m) => cl -> SafeClock m cl
safeClock unhoistedClock =
  HoistClock
    { unhoistedClock
    , monadMorphism = fmap (either absurd id) . runExceptT
    }

-- * 'Single' clock

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
  initClock Single {singleTag, getTime, exception} = hoistS (errorT . runExceptT) $ runAutomatonExcept $ do
    initTime <- once_ getTime
    step_ (initTime, singleTag)
    return exception
    where
      errorT :: (MonadError e m) => m (Either e a) -> m a
      errorT = (>>= liftEither)

-- * 'DelayException'

{- | Catch an exception in clock @cl@ and throw it after one time step.

This is particularly useful if you want to give your signal network a chance to save its current state in some way.
-}
type DelayException m time cl e1 e2 = CatchClock cl e1 (Single m time e1 e2)

-- | Construct a 'DelayException' clock.
delayException ::
  (Monad m, Clock (ExceptT e1 m) cl, MonadError e2 m) =>
  -- | The clock that will throw an exception @e@
  cl ->
  -- | How to transform the exception into the new exception that will be thrown later
  (e1 -> e2) ->
  -- | How to measure the current time
  m (Time cl) ->
  DelayException m (Time cl) cl e1 e2
delayException cl handler mTime = CatchClock cl $ \e -> Single e mTime $ handler e

-- | Like 'delayException', but the exception thrown by @cl@ and by the @DelayException@ clock are the same.
delayException' :: (Monad m, MonadError e m, Clock (ExceptT e m) cl) => cl -> m (Time cl) -> DelayException m (Time cl) cl e e
delayException' cl = delayException cl id

-- | Catch an 'IO' 'Exception', and throw it after one time step.
type DelayMonadIOException m cl e1 e2 = DelayException m UTCTime (ExceptClock cl e1) e1 e2

-- | Build a 'DelayMonadIOException'. The time will be measured using the system time.
delayMonadIOException :: (Exception e1, MonadIO m, MonadError e2 m, Clock IO cl, Time cl ~ UTCTime) => cl -> (e1 -> e2) -> DelayMonadIOException m cl e1 e2
delayMonadIOException cl handler = delayException (ExceptClock cl) handler $ liftIO getCurrentTime

-- | 'DelayMonadIOException' specialised to 'IOError'.
type DelayMonadIOError m cl e = DelayMonadIOException m cl IOError e

-- | 'delayMonadIOException' specialised to 'IOError'.
delayMonadIOError :: (Exception e, MonadError e m, MonadIO m, Clock IO cl, Time cl ~ UTCTime) => cl -> (IOError -> e) -> DelayMonadIOError m cl e
delayMonadIOError = delayMonadIOException

-- | Like 'delayMonadIOError', but throw the error without transforming it.
delayMonadIOError' :: (MonadError IOError m, MonadIO m, Clock IO cl, Time cl ~ UTCTime) => cl -> DelayMonadIOError m cl IOError
delayMonadIOError' cl = delayMonadIOError cl id

{- | 'DelayMonadIOException' specialised to the monad @'ExceptT' e2 'IO'@.

This is sometimes helpful when the type checker complains about an ambigous monad type variable.
-}
type DelayIOException cl e1 e2 = DelayException (ExceptT e2 IO) UTCTime (ExceptClock cl e1) e1 e2

-- | 'delayMonadIOException' specialised to the monad @'ExceptT' e2 'IO'@.
delayIOException :: (Exception e1, Clock IO cl, Time cl ~ UTCTime) => cl -> (e1 -> e2) -> DelayIOException cl e1 e2
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
