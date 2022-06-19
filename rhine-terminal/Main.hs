{- | Example application for the @terminal@ wrIOer. -}


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (putChar)

-- rhine
import Control.Monad.Schedule ()

import Data.Text (Text)
import qualified Data.Text as T
import System.Terminal
import System.Terminal.Internal
import FRP.Rhine
import System.IO hiding (putChar)

import FRP.Rhine.Terminal (TerminalEventClock(..), terminalConcurrently)
import System.Exit (exitSuccess)


type App = AppT IO
type AppT = TerminalT LocalTerminal

-- Clocks

data Input = Char Char | Space | Backspace | Enter

type InputClock = SelectClock (TerminalEventClock LocalTerminal) Input

keyClock :: LocalTerminal -> InputClock
keyClock term = SelectClock { mainClock = TerminalEventClock term, select = selectKey }
  where
    selectKey :: Tag (TerminalEventClock LocalTerminal) -> Maybe Input
    selectKey = \case
      Right (KeyEvent (CharKey k) _) -> Just (Char k)
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

type BeatClock = LiftClock IO AppT (Millisecond 1000)


type AppClock = ParallelClock App (ParallelClock App InputClock BeatClock) SignalClock

-- Rhines

keySource :: LocalTerminal -> Rhine App InputClock () Input
keySource term = tagS @@ keyClock term

beatSource :: Rhine App BeatClock () Text
beatSource = (flip T.cons " > " . (cycle " ." !!) <$> count) @@ liftClock waitClock

signalSource :: LocalTerminal ->Rhine App SignalClock () Interrupt
signalSource term = tagS @@ signalClock term

data Actions = Input Input
             | Prompt Text
             | Signal Interrupt

sources :: LocalTerminal
        -> Rhine App InputClock () Input
        -> Rhine App BeatClock () Text
        -> Rhine App SignalClock () Interrupt
        -> Rhine App AppClock () Actions
sources term input prompt signal =
  ( ( input ++@ terminalConcurrently term @++ prompt )
            ++@ terminalConcurrently term @++ signal
  ) @>>^ \case
            Left (Left i) -> Input i
            Left (Right p) -> Prompt p
            Right s -> Signal s

-- ClSFs

display :: ClSF App cl Actions ()
display = arrMCl $ \case
  Input i ->
    case i of
      Char c -> putChar c >> flush
      Space -> putChar ' ' >> flush
      Backspace -> moveCursorBackward 1 >> deleteChars 1 >> flush
      Enter -> putLn >> flush

  Prompt prmpt -> do
    Position _ column <- getCursorPosition
    if column /= 0 then do
      moveCursorBackward column
      putText prmpt
      setCursorColumn column
    else putText prmpt
    flush

  Signal i -> do
    putLn
    putStringLn $ show i ++ ": exiting program."
    flush
    liftIO exitSuccess


-- Rhines

mainRhine :: LocalTerminal -> Rhine App AppClock () ()
mainRhine term = sources term (keySource term) beatSource (signalSource term) @>-^ display

-- Main

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  withTerminal $ \term -> runTerminalT (flow $ mainRhine term) term


