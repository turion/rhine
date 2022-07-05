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

import Prelude hiding (putChar)

-- rhine
import Control.Monad.Schedule ()

import Data.Text (Text)
import qualified Data.Text as T
import System.Terminal
import System.Terminal.Internal
import FRP.Rhine
import System.IO hiding (putChar)

import FRP.Rhine.Terminal (TerminalEventClock(..))
import FRP.Rhine.FSNotify (WatchDirEventClock(..))
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.FSNotify
import qualified System.FSNotify as FSNotify
import System.Exit (exitSuccess)

-- Clocks

data Input = Char Char Modifiers | Space | Backspace | Enter

type InputClock = SelectClock (TerminalEventClock LocalTerminal) Input

keyClock :: LocalTerminal -> InputClock
keyClock term = SelectClock { mainClock = TerminalEventClock term, select = selectKey }
  where
    selectKey :: Tag (TerminalEventClock LocalTerminal) -> Maybe Input
    selectKey = \case
      Right (KeyEvent (CharKey k) m) -> Just (Char k m)
      Right (KeyEvent SpaceKey _) -> Just Space
      Right (KeyEvent BackspaceKey _) -> Just Backspace
      Right (KeyEvent EnterKey _) -> Just Enter
      _ -> Nothing

type SignalClock = SelectClock (TerminalEventClock LocalTerminal) Interrupt

signalClock :: LocalTerminal -> SignalClock
signalClock term = SelectClock { mainClock = TerminalEventClock term, select = selectInterrupt }
  where
    selectInterrupt :: Tag (TerminalEventClock LocalTerminal) -> Maybe Interrupt
    selectInterrupt = \case
      Left i -> Just i
      _ -> Nothing

type BeatClock = Millisecond 1000

type AppClock = ParallelClock IO (ParallelClock IO (ParallelClock IO InputClock SignalClock) BeatClock) WatchDirEventClock

-- Rhines

keySource :: LocalTerminal -> Rhine IO InputClock () Input
keySource term = tagS @@ keyClock term

beatSource :: Rhine IO BeatClock () Text
beatSource = (flip T.cons " > " . (cycle " ." !!) <$> count) @@ waitClock

signalSource :: LocalTerminal -> Rhine IO SignalClock () Interrupt
signalSource term = tagS @@ signalClock term

watchDirSource :: WatchManager -> FilePath -> ActionPredicate -> Rhine IO WatchDirEventClock () FSNotify.Event
watchDirSource mgr fp ap = tagS @@ WatchDirEventClock mgr fp ap

data Actions = Input Input
             | Prompt Text
             | Signal Interrupt
             | Event FSNotify.Event

sources :: Rhine IO InputClock () Input
        -> Rhine IO BeatClock () Text
        -> Rhine IO SignalClock () Interrupt
        -> Rhine IO WatchDirEventClock () FSNotify.Event
        -> Rhine IO AppClock () Actions
sources input prompt signal watchdir =
  ( ( ( input ++@ schedSelectClocks @++ signal )
              ++@ concurrently @++ prompt )
              ++@ concurrently @++ watchdir
  ) @>>^ \case
            Left (Left (Left i)) -> Input i
            Left (Left (Right s)) -> Signal s
            Left (Right p) -> Prompt p
            Right w -> Event w

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

display :: LocalTerminal -> ClSF IO cl Actions ()
display term = arrMCl $ (flip runTerminalT term .) $ \case
  Input i ->
    case i of
      -- Don't display Ctrl-J https://github.com/lpeterse/haskell-terminal/issues/17
      Char c m
        | c /= 'J' && m /= ctrlKey -> putChar c >> flush
        | otherwise -> pure ()
      Space -> putChar ' ' >> flush
      Backspace -> moveCursorBackward 1 >> deleteChars 1 >> flush
      Enter -> putLn >> flush

  Prompt prmpt -> changePrompt prmpt

  Signal i -> do
    putLn
    putStringLn $ show i ++ ": exiting program."
    flush
    liftIO exitSuccess

  Event _ -> do
    changePrompt "*"
    -- putStringLn $ "Event: " ++ show e
    flush


-- Rhines

mainRhine :: LocalTerminal -> WatchManager -> Handle -> Rhine IO AppClock () ()
mainRhine term mgr history = sources (keySource term) beatSource (signalSource term) (watchDirSource mgr "." (const True)) @>-^ display term

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
        flow $ mainRhine term mgr history
