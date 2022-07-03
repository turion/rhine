{- | Example application for the @rhine-terminal@ library. -}

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
import System.Exit (exitSuccess)

-- Clocks

data Input = Char Char Modifiers
           | Space | Backspace | Enter
           | Exit

type InputClock = SelectClock (TerminalEventClock LocalTerminal) Input

inputClock :: LocalTerminal -> InputClock
inputClock term = SelectClock { mainClock = TerminalEventClock term, select = selectKey }
  where
    selectKey :: Tag (TerminalEventClock LocalTerminal) -> Maybe Input
    selectKey = \case
      Right (KeyEvent (CharKey k) m) -> Just (Char k m)
      Right (KeyEvent SpaceKey _) -> Just Space
      Right (KeyEvent BackspaceKey _) -> Just Backspace
      Right (KeyEvent EnterKey _) -> Just Enter
      Left _ -> Just Exit
      _ -> Nothing

type PromptClock = Millisecond 1000

type AppClock = ParallelClock IO InputClock PromptClock

-- ClSFs

inputSource :: ClSF IO InputClock () Input
inputSource = tagS

promptSource :: ClSF IO PromptClock () Text
promptSource = flip T.cons " > " . (cycle " ." !!) <$> count

inputSink :: LocalTerminal -> ClSF IO cl Input ()
inputSink term = arrMCl $ (flip runTerminalT term .) $ \case
  -- Don't display Ctrl-J https://github.com/lpeterse/haskell-terminal/issues/17
  Char c m
    | c /= 'J' && m /= ctrlKey -> putChar c >> flush
    | otherwise -> pure ()
  Space -> putChar ' ' >> flush
  Backspace -> moveCursorBackward 1 >> deleteChars 1 >> flush
  Enter -> putLn >> flush
  Exit -> do
    putLn
    putStringLn "Exiting program."
    flush
    liftIO exitSuccess

promptSink :: LocalTerminal -> ClSF IO cl Text ()
promptSink term = arrMCl $ (flip runTerminalT term .) $ \prmpt -> do
  Position _ column <- getCursorPosition
  if column /= 0 then do
    moveCursorBackward column
    putText prmpt
    setCursorColumn column
  else putText prmpt
  flush


-- Rhines

mainRhine :: LocalTerminal -> Rhine IO AppClock () ()
mainRhine term = inputRhine ||@ concurrently @|| promptRhine
  where
    inputRhine :: Rhine IO InputClock () ()
    inputRhine = inputSource >-> inputSink term @@ inputClock term

    promptRhine :: Rhine IO PromptClock () ()
    promptRhine = promptSource >-> promptSink term @@ waitClock

-- Main

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  withTerminal $ \term -> flow $ mainRhine term
