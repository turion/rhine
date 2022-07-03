{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Prelude hiding (putChar)
import FRP.Rhine.Terminal
import System.Terminal
import System.Terminal.Internal
import FRP.Rhine
import GHC.Conc (retry, readTVarIO, atomically)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)

--stm
import Control.Concurrent.STM.TQueue

import Test.Hspec
import Data.Text (singleton)

type KeyClock = SelectClock TerminalEventClock Char

keyClock :: KeyClock
keyClock = SelectClock { mainClock = TerminalEventClock , select }
  where
    select :: Tag TerminalEventClock -> Maybe Char
    select = \case
      Right (KeyEvent (CharKey k) _) -> Just k
      _ -> Nothing

defaultSettings :: TQueue Event -> VirtualTerminalSettings
defaultSettings eventQueue = VirtualTerminalSettings
    { virtualType         = "xterm"
    , virtualWindowSize   = pure (Size 3 10)
    , virtualEvent        = readTQueue eventQueue
    , virtualInterrupt    = retry
    }

displayDot :: MonadScreen m => ClSF m KeyClock () ()
displayDot = tagS >-> arrMCl (\_ -> do
  putChar '.'
  flush)

testRhine :: Terminal t => Rhine (TerminalT t IO) KeyClock () ()
testRhine = displayDot @@ keyClock

charEvent :: Terminal t => TQueue Event -> t -> Char -> IO ()
charEvent eventQueue terminal char = do
  termCommand terminal $ PutText $ singleton char
  atomically $ writeTQueue eventQueue $ KeyEvent (CharKey char) mempty

main :: IO ()
main = hspec $ do
    describe "rhine-terminal with VirtualTerminal" $ do
      it "replaces virtual inputs by dots" $ do
        eventQueue <- newTQueueIO
        withVirtualTerminal (defaultSettings eventQueue) $ \t -> do
          void $ liftIO $ forkIO $ flowTerminal t testRhine
          charEvent eventQueue t '1'
          threadDelay $ 1000 * 1000
          charEvent eventQueue t '2'
          threadDelay $ 1000 * 1000
          charEvent eventQueue t '3'
          threadDelay $ 1000 * 1000
          readTVarIO (virtualWindow t) `shouldReturn` expWindow
        where
          expWindow =
              [ "1.2.3.    "
              , "          "
              , "          " ]
