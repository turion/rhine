{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Example application for the @rhine-terminal@ library.
module Main where

-- base

import System.Exit (exitSuccess)
import System.IO hiding (putChar)
import Prelude hiding (putChar)

-- text
import Data.Text (Text)
import qualified Data.Text as T

-- terminal
import System.Terminal
import System.Terminal.Internal

-- rhine
import FRP.Rhine

-- rhine-terminal
import FRP.Rhine.Terminal

type App = TerminalT LocalTerminal IO

-- Clocks

data Input
  = Char Char Modifiers
  | Space
  | Backspace
  | Enter
  | Exit

type InputClock = SelectClock TerminalEventClock Input

inputClock :: InputClock
inputClock =
  SelectClock
    { mainClock = TerminalEventClock
    , select = \case
        Right (KeyEvent (CharKey k) m)
          -- Don't display Ctrl-J https://github.com/lpeterse/haskell-terminal/issues/17
          | k /= 'J' || m /= ctrlKey -> Just (Char k m)
        Right (KeyEvent SpaceKey _) -> Just Space
        Right (KeyEvent BackspaceKey _) -> Just Backspace
        Right (KeyEvent EnterKey _) -> Just Enter
        Left _ -> Just Exit
        _ -> Nothing
    }

type PromptClock = LiftClock IO (TerminalT LocalTerminal) (Millisecond 1000)

type AppClock = ParallelClock App InputClock PromptClock

-- ClSFs

inputSource :: ClSF App InputClock () Input
inputSource = tagS

promptSource :: ClSF App PromptClock () Text
promptSource = flip T.cons " > " . (cycle " ." !!) <$> count

inputSink :: ClSF App cl Input ()
inputSink = arrMCl $ \case
  Char c _ -> putChar c >> flush
  Space -> putChar ' ' >> flush
  Backspace -> moveCursorBackward 1 >> deleteChars 1 >> flush
  Enter -> putLn >> changePrompt "  > " >> flush
  Exit -> do
    putLn
    putStringLn "Exiting program."
    flush
    liftIO exitSuccess

changePrompt :: MonadScreen m => Text -> m ()
changePrompt prmpt = do
  Position _ column <- getCursorPosition
  if column /= 0
    then do
      moveCursorBackward column
      putText prmpt
      setCursorColumn column
    else putText prmpt
  flush

promptSink :: ClSF App cl Text ()
promptSink = arrMCl changePrompt

-- Rhines

mainRhine :: Rhine App AppClock () ()
mainRhine = inputRhine ||@ terminalConcurrently @|| promptRhine
  where
    inputRhine :: Rhine App InputClock () ()
    inputRhine = inputSource >-> inputSink @@ inputClock

    promptRhine :: Rhine App PromptClock () ()
    promptRhine = promptSource >-> promptSink @@ liftClock waitClock

-- Main

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  withTerminal $ \term -> flowTerminal term mainRhine
