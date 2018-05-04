{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module FRP.Rhine.SDL where

-- base
import Data.Word (Word32)
import Foreign.C.Types (CInt)

-- text
import Data.Text (Text)

-- transformers
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer

-- sdl2
import SDL
import qualified SDL


-- rhine
import FRP.Rhine


-- TODO Allow many more options
-- TODO That's only the event clock! Where's the continuous graphics clock?
data SDLClock = SDLClock
  { windowTitle  :: Text -- ^ The title of the window that will be created
  , eventTimeout :: Maybe CInt -- ^ The maximum duration the clock should wait for an event, in milliseconds.
  }

defaultSDLClock :: SDLClock
defaultSDLClock  = SDLClock
  { windowTitle  = "default"
  , eventTimeout = Nothing
  }

instance TimeDomain Word32 where
  type Diff Word32 = Word32
  diffTime = (-)

-- TODO Put in 'MaybeT'? This is only necessary if there's cleanup stuff to do,
-- otherwise the user can just do it themselves by hoisting?

-- TODO Check out whether MonadIO is sufficient
instance MonadIO m => Clock m SDLClock where
  -- | The time since the initialisation of the SDL system, in milliseconds.
  type TimeDomainOf SDLClock = Word32
  type Tag          SDLClock = (Window, Surface, Maybe Event)
  startClock SDLClock {..} = do
    initializeAll
    window   <- createWindow windowTitle defaultWindow
    --renderer <- createRenderer window (-1) defaultRenderer
    --let waitEventMethod = maybe (Just <$> waitEvent) waitEventTimeout eventTimeout
    let runningClock = proc _ -> do
          --mEvent  <- arrM_ waitEventMethod -< ()
          ticksMS <- arrM_ ticks           -< ()
          --arrM_ (present renderer) -< () -- Present what has been drawn since the last tick
          --arrM_ (rendererDrawColor renderer $= V4 0 0 255 255) -< renderer

          --arrM_ (clear renderer) -< () -- Clear the back buffer to allow for new drawing
          arrM_ (updateWindowSurface window) -< ()
          screen  <- arrM_ (getWindowSurface window) -< ()
          arrM_ (\s -> surfaceFillRect Nothing $ V4 0 0 0 0)
          returnA                          -< (ticksMS, (window, screen, Nothing))
    initialTime <- ticks
    _ <- getWindowSurface window
    --clear renderer
    return (runningClock, initialTime)

-- TODO Should one renderer correspond to one window?

-- TODO How to do an event clock in the same thread?

{-
-- TODO Wrap all SDL methods here that need a renderer?

-- TODO Would want typestate to make sure this is only called once
-- TODO Can we put this into the runningClock?
clearAndPresent :: SyncSF IO SDLClock () ()
clearAndPresent = proc _ -> do
  renderer <- rendererS -< ()
  arrMSync clear        -< renderer
  arrMSync present      -< renderer
-}

data SDLAudioClock = SDLAudioClock

instance MonadIO m => Clock m SDLAudioClock where
  type TimeDomainOf SDLAudioClock = Word32
  type Tag          SDLAudioClock = ()
  startClock _ = undefined

type SDLWriteAudio m = WriterT Int m

{-

audio :: SyncSF (WriterT (Sum Int) IO) AudioClock

handleAudio
-}
