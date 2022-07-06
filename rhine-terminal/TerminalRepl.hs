{- | Example application for the @terminal@ wrapper. -}


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

import Prelude hiding (putChar)

-- rhine
import Control.Monad.Schedule ()

import Data.Text (Text)
import qualified Data.Text as T
import System.Terminal
import System.Terminal.Internal
import FRP.Rhine
import System.IO hiding (putChar)

import FRP.Rhine.Terminal (TerminalEventClock(..), flowTerminal, terminalConcurrently)
import FRP.Rhine.FSNotify (WatchDirEventClock(..))
import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Control.Monad.Trans.State
import System.FSNotify
import qualified System.FSNotify as FSNotify
import System.Exit (exitSuccess)


type Driver = TerminalT LocalTerminal IO
type App = StateT AppState Driver

-- State

data AppState = AppState
  { statePrompt :: Text
  , stateCounter :: Int
  }

-- Clocks

data Input = Char Char Modifiers | Space | Backspace | Enter

type InputClock = SelectClock TerminalEventClock Input

keyClock :: InputClock
keyClock = SelectClock { mainClock = TerminalEventClock, select }
  where
    select :: Tag TerminalEventClock -> Maybe Input
    select = \case
      Right (KeyEvent (CharKey k) m) -> Just (Char k m)
      Right (KeyEvent SpaceKey _) -> Just Space
      Right (KeyEvent BackspaceKey _) -> Just Backspace
      Right (KeyEvent EnterKey _) -> Just Enter
      _ -> Nothing

type SignalClock = SelectClock TerminalEventClock Interrupt

signalClock :: SignalClock
signalClock = SelectClock { mainClock = TerminalEventClock, select }
  where
    select :: Tag TerminalEventClock -> Maybe Interrupt
    select = \case
      Left i -> Just i
      _ -> Nothing

type BeatClock = LiftClock IO (TerminalT LocalTerminal) (Millisecond 1000)

type AppClock = ParallelClock App (ParallelClock App (ParallelClock App InputClock SignalClock) BeatClock) WatchDirEventClock

type DriverClock = ParallelClock Driver (ParallelClock Driver (ParallelClock Driver InputClock SignalClock) BeatClock) WatchDirEventClock

-- Rhines

keySource :: Rhine Driver InputClock () Input
keySource = tagS @@ keyClock

signalSource :: Rhine Driver SignalClock () Interrupt
signalSource = tagS @@ signalClock

-- beatSource :: Rhine App PromptClock () Text
-- beatSource = (flip T.cons " > " . (cycle " ." !!) <$> count) @@ liftClock waitClock

beatSource :: Rhine Driver BeatClock () Bool
beatSource = (cycle [True, False] !!) <$> count @@ liftClock waitClock

watchDirSource :: MonadIO m => WatchManager -> FilePath -> ActionPredicate -> Rhine m WatchDirEventClock () FSNotify.Event
watchDirSource mgr fp ap = tagS @@ WatchDirEventClock mgr fp ap

data Response
  = Input Input
  | Beat Bool
  | Signal Interrupt
  | Event FSNotify.Event

data Request
  = Display Input
  | Prompt Text
  | Exit

sources
  :: Rhine Driver InputClock () Input
  -> Rhine Driver BeatClock () Bool
  -> Rhine Driver SignalClock () Interrupt
  -> Rhine Driver WatchDirEventClock () FSNotify.Event
  -> Rhine Driver DriverClock () Response
sources input beat signal watchdir =
  ( ( ( input ++@ schedSelectClocks @++ signal )
              ++@ terminalConcurrently @++ beat )
              ++@ terminalConcurrently @++ watchdir
  ) @>>^ \case
            Left (Left (Left i)) -> Input i
            Left (Left (Right s)) -> Signal s
            Left (Right p) -> Beat p
            Right w -> Event w



-- ClSFs

actuate
  :: (MonadScreen m, MonadIO m)
  => ClSF m cl Request ()
actuate = arrMCl $ \case
  Display i -> case i of
    -- Don't display Ctrl-J https://github.com/lpeterse/haskell-terminal/issues/17
    Char c m  -> when (c /= 'J' && m /= ctrlKey) $ putChar c >> flush
    Space -> putChar ' ' >> flush
    Backspace -> moveCursorBackward 1 >> deleteChars 1 >> flush
    Enter  -> putLn >> changePrompt "  > " >> flush

  Prompt prmpt -> changePrompt prmpt

  Exit -> do
    putLn
    putStringLn "Exiting program."
    flush
    liftIO exitSuccess

  -- Signal i -> do
  --   putLn
  --   putStringLn $ show i ++ ": exiting program."
  --   flush
  --   liftIO exitSuccess

  -- Event _ -> do
  --   changePrompt "*"
  --   -- putStringLn $ "Event: " ++ show e
  --   flush

-- Application

application :: ClSF Driver DriverClock Response Request
application = undefined

-- Utilities

changePrompt :: MonadScreen m => Text -> m ()
changePrompt prmpt = do
  Position _ column <- getCursorPosition
  if column /= 0 then do
    moveCursorBackward column
    putText prmpt
    setCursorColumn column
  else putText prmpt
  flush


-- Rhines

mainRhine :: WatchManager -> Handle -> Text -> Rhine Driver DriverClock () ()
mainRhine mgr history suffix
  = sources keySource beatSource signalSource (watchDirSource mgr "." (const True)) @>-^ application @>-^ actuate

-- Persistence

-- initHistory :: IO ()
-- initHistory = pure ()

-- Main

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  withManager $ \mgr -> do
    withFile ".history" ReadWriteMode $ \history ->
      withTerminal $ \term -> do
        liftIO $ print (termType term)
        flowTerminal term $ mainRhine mgr history " > "
