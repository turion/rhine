{- | Example application for the @terminal@ wrapper. -}

{-# LANGUAGE Arrows #-}
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

import System.Terminal hiding (Interrupt)
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
  { statePromptOffset :: Int
  , stateCounter :: Int
  }

-- Clocks

data Input
  = Char Char Modifiers
  | Backspace
  | Enter
  deriving (Show)

type InputClock = SelectClock TerminalEventClock Input

keyClock :: InputClock
keyClock = SelectClock { mainClock = TerminalEventClock, select }
  where
    select :: Tag TerminalEventClock -> Maybe Input
    select = \case
      Right (KeyEvent (CharKey k) m)
        -- Don't display Ctrl-J https://github.com/lpeterse/haskell-terminal/issues/17
        | (k /= 'J' && m /= ctrlKey) -> Just (Char k m)
      Right (KeyEvent BackspaceKey _) -> Just Backspace
      Right (KeyEvent EnterKey _) -> Just Enter
      _ -> Nothing

type ControlClock = SelectClock TerminalEventClock Signal

controlClock :: ControlClock
controlClock = SelectClock { mainClock = TerminalEventClock, select }
  where
    select :: Tag TerminalEventClock -> Maybe Signal
    select = \case
      Right (KeyEvent EnterKey m) | m == shiftKey -> Just ShiftEnter
      Left _ -> Just Interrupt
      _ -> Nothing

type BeatClock = LiftClock IO (TerminalT LocalTerminal) (Millisecond 1000)

type AppClock = ParallelClock App (ParallelClock App (ParallelClock App InputClock ControlClock) BeatClock) WatchDirEventClock

type DriverClock = ParallelClock Driver (ParallelClock Driver (ParallelClock Driver InputClock ControlClock) BeatClock) WatchDirEventClock

-- Rhines

keySource :: Rhine Driver InputClock () Input
keySource = tagS @@ keyClock

controlSource :: Rhine Driver ControlClock () Signal
controlSource = tagS @@ controlClock

-- beatSource :: Rhine App PromptClock () Text
-- beatSource = (flip T.cons " > " . (cycle " ." !!) <$> count) @@ liftClock waitClock

beatSource :: Rhine Driver BeatClock () Bool
beatSource = ((cycle [True, False] !!) <$> count) @@ liftClock waitClock

watchDirSource :: MonadIO m => WatchManager -> FilePath -> ActionPredicate -> Rhine m WatchDirEventClock () FSNotify.Event
watchDirSource mgr fp ap = tagS @@ WatchDirEventClock mgr fp ap

data Signal
  = Interrupt
  | ShiftEnter

data Response
  = Input Input
  | Beat Bool
  | Control Signal
  | WatchDirEvent FSNotify.Event

data Request
  = Display Input
  | Prompt Text
  | Log Text
  | Send
  | Exit

sources
  :: Rhine Driver InputClock () Input
  -> Rhine Driver BeatClock () Bool
  -> Rhine Driver ControlClock () Signal
  -> Rhine Driver WatchDirEventClock () FSNotify.Event
  -> Rhine Driver DriverClock () Response
sources input beat control watchdir =
  ( ( ( input ++@ schedSelectClocks @++ control )
              ++@ terminalConcurrently @++ beat )
              ++@ terminalConcurrently @++ watchdir
  ) @>>^ \case
            Left (Left (Left i)) -> Input i
            Left (Left (Right s)) -> Control s
            Left (Right p) -> Beat p
            Right w -> WatchDirEvent w

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

-- ClSFs

actuate
  :: (MonadScreen m, MonadIO m)
  => ClSF m cl [Request] ()
actuate = arrMCl go
  where
    go [] = return ()
    go (x:xs) = do
      case x of
        Display i -> case i of
          Char c _  -> putChar c >> flush
          Backspace -> moveCursorBackward 1 >> deleteChars 1 >> flush
          Enter -> putLn >> changePrompt "     " >> flush

        Log l -> do
          (Size h w) <- getWindowSize
          -- setCursorColumn (w - T.length l)
          putText $ T.pack $ show w
          putText $ T.pack $ show h
          saveCursor
          setCursorColumn 40
          putText l
          restoreCursor
          flush

        Send -> putLn >> changePrompt "   > " >> flush

        Prompt prmpt -> changePrompt prmpt

        Exit -> do
          putLn
          putStringLn "Exiting program."
          flush
          liftIO exitSuccess
      go xs

  -- Signal i -> do
  --   putLn
  --   putStringLn $ show i ++ ": exiting program."
  --   flush
  --   liftIO exitSuccess

  -- Event _ -> do
  --   changePrompt "*"
  --   -- putStringLn $ "Event: " ++ show e
  --   flush

logR :: Response -> Request
logR (Input (Char c _)) = Log $ T.snoc "Input " c
logR (Control Interrupt) = Log $ "Control " <> "Interrupt"
logR (Control ShiftEnter) = Log $ "Control " <> "ShiftEnter"
logR _ = Log "*"

-- Application

repl :: Monad m => ClSF m cl Response [Request]
repl = proc input -> do
  -- advanceJointState -< input
  case input of
    Input i -> returnA -< [Display i, logR input]
    Beat b -> case b of
      True -> returnA -< [Prompt " . > "]
      False -> returnA -< [Prompt "   > "]
    WatchDirEvent _ -> returnA -< [Prompt " * > "]
    Control Interrupt -> returnA -< [Exit]
    Control ShiftEnter -> returnA -< [Exit]


-- Rhines

mainRhine :: WatchManager -> Handle -> Text -> Rhine Driver DriverClock () ()
mainRhine mgr history suffix
  = sources keySource beatSource controlSource (watchDirSource mgr "." (const True)) @>-^ repl @>-^ actuate

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
