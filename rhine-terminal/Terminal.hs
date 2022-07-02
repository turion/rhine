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
import System.Exit (exitSuccess)

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

type BeatClock = Millisecond 1000

type AppClock = ParallelClock IO (ParallelClock IO InputClock SignalClock) BeatClock

-- Rhines

keySource :: LocalTerminal -> Rhine IO InputClock () Input
keySource term = tagS @@ keyClock term

beatSource :: Rhine IO BeatClock () Text
beatSource = (flip T.cons " > " . (cycle " ." !!) <$> count) @@ waitClock

signalSource :: LocalTerminal ->Rhine IO SignalClock () Interrupt
signalSource term = tagS @@ signalClock term

data Actions = Input Input
             | Prompt Text
             | Signal Interrupt

sources :: Rhine IO InputClock () Input
        -> Rhine IO BeatClock () Text
        -> Rhine IO SignalClock () Interrupt
        -> Rhine IO AppClock () Actions
sources input prompt signal =
  ( ( input ++@ schedSelectClocks @++ signal )
            ++@ concurrently @++ prompt
  ) @>>^ \case
            Left (Left i) -> Input i
            Left (Right s) -> Signal s
            Right p -> Prompt p

-- ClSFs

display :: LocalTerminal -> ClSF IO cl Actions ()
display term = arrMCl $ (flip runTerminalT term .) $ \case
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

mainRhine :: LocalTerminal -> Rhine IO AppClock () ()
mainRhine term = sources (keySource term) beatSource (signalSource term) @>-^ display term

-- Main

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  withTerminal $ \term -> flow $ mainRhine term
