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
{-# LANGUAGE NamedFieldPuns #-}

-- base
import Prelude hiding (putChar)
import System.Exit (exitSuccess)
import System.IO hiding (putChar)
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
    ( TerminalEventClock(..), flowTerminal, terminalConcurrently )

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
inputClock = SelectClock
  { mainClock = TerminalEventClock
  , select }
    where
      select :: Tag TerminalEventClock -> Maybe Input
      select (Right (KeyEvent (CharKey k) m))  = Just (Char k m)
      select (Right (KeyEvent SpaceKey _))     = Just Space
      select (Right (KeyEvent BackspaceKey _)) = Just Backspace
      select (Right (KeyEvent EnterKey _))     = Just Enter
      select (Left _)                          = Just Exit
      select _                                 = Nothing

type PromptClock = LiftClock IO (TerminalT LocalTerminal) (Millisecond 1000)

type AppClock = ParallelClock App InputClock PromptClock

-- ClSFs

inputSource :: ClSF App InputClock () Input
inputSource = tagS

promptSource :: ClSF App PromptClock () Text
promptSource = flip T.cons " > " . (cycle " ." !!) <$> count

inputSink ::  ClSF App cl Input ()
inputSink = arrMCl $ \input -> do
  case input of
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

promptSink :: ClSF App cl Text ()
promptSink = arrMCl $ \prmpt -> do
  Position _ column <- getCursorPosition
  if column /= 0 then do
    moveCursorBackward column
    putText prmpt
    setCursorColumn column
  else putText prmpt
  flush

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
