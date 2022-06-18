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


type App = AppT IO
type AppT = TerminalT LocalTerminal

-- Clocks

type KeyClock = SelectClock TerminalEventClock Char

keyClock :: LocalTerminal -> KeyClock
keyClock term = SelectClock { mainClock = TerminalEventClock term, select = selectChar }
  where
    selectChar :: Tag TerminalEventClock -> Maybe Char
    selectChar = \case
      Right (KeyEvent (CharKey k) _) -> Just k
      _ -> Nothing

type BeatClock = Millisecond 1000

-- ClSFs

beat :: MonadScreen m => ClSF m (LiftClock IO AppT BeatClock) () ()
beat = (flip T.cons " > " . (cycle " ." !!) <$> count) >-> displayBeat

displayBeat :: MonadScreen m => ClSF m cl Text ()
displayBeat = arrMCl $ \prompt ->  do
  Position _ column <- getCursorPosition
  if column /= 0 then do
    moveCursorBackward column
    putText prompt
    setCursorColumn column
  else putText prompt
  flush

key :: MonadScreen m => ClSF m KeyClock () ()
key = tagS >-> displayKey

displayKey :: MonadScreen m => ClSF m KeyClock Char ()
displayKey = arrMCl $ \k -> do
  putChar k
  flush

-- Rhines

type DisplayClock = ParClock App (LiftClock IO AppT BeatClock)  KeyClock

mainRhine :: LocalTerminal -> Rhine App DisplayClock () ()
mainRhine term = beatRhine ||@ terminalConcurrently term @|| keyRhine
  where
    beatRhine :: Rhine App (LiftClock IO (TerminalT LocalTerminal) BeatClock) () ()
    beatRhine = beat @@ liftClock waitClock

    keyRhine :: Rhine App KeyClock () ()
    keyRhine = key @@ keyClock term

-- Main

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  -- withTerminal $ \term -> runTerminalT (flow $ mainRhine term) term
  withTerminal $ \term -> runTerminalT (flow $ mainRhine' term) term

------------
--
-- Alternate

beat' :: ClSF App cl () Text
beat' = flip T.cons " > " . (cycle " ." !!) <$> count

key' :: ClSF App KeyClock () Char
key' = tagS

display' :: ClSF App cl (Either Text Char) ()
display' = arrMCl $ \case
  Left prompt -> do
    Position _ column <- getCursorPosition
    if column /= 0 then do
      moveCursorBackward column
      putText prompt
      setCursorColumn column
    else putText prompt
    flush
  Right k -> do
    putChar k
    flush

-- Rhine of sources

sensor' :: LocalTerminal -> Rhine App DisplayClock () (Either Text Char)
sensor' term = beat' @@ liftClock waitClock ++@ terminalConcurrently term @++ key' @@ keyClock term

mainRhine' :: LocalTerminal -> Rhine App DisplayClock () ()
mainRhine' term = sensor' term @>-^ display'
