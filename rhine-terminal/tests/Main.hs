{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

-- base
import Prelude hiding (putChar)
import GHC.Conc (retry, readTVarIO, atomically)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)

-- rhine
import FRP.Rhine
import FRP.Rhine.Terminal

-- terminal
import System.Terminal
import System.Terminal.Internal

-- text
import Data.Text (singleton)

-- stm
import Control.Concurrent.STM.TQueue

-- hspec
import Test.Hspec

type KeyClock = SelectClock TerminalEventClock Char

keyClock :: KeyClock
keyClock = SelectClock { mainClock = TerminalEventClock , select }
  where
    select :: Tag TerminalEventClock -> Maybe Char
    select (Right (KeyEvent (CharKey k) _)) = Just k
    select _                                = Nothing

defaultSettings :: TQueue Event -> VirtualTerminalSettings
defaultSettings eventQueue = VirtualTerminalSettings
  { virtualType         = "xterm"
  , virtualWindowSize   = pure (Size 3 10)
  , virtualEvent        = readTQueue eventQueue
  , virtualInterrupt    = retry
  }

displayDot :: MonadScreen m => ClSF m KeyClock () ()
displayDot = constMCl $ do
  putChar '.'
  flush

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
        threadDelay $ 200 * 1000
        charEvent eventQueue t '2'
        threadDelay $ 200 * 1000
        charEvent eventQueue t '3'
        threadDelay $ 200 * 1000
        readTVarIO (virtualWindow t) `shouldReturn` expWindow
      where
        expWindow =
            [ "1.2.3.    "
            , "          "
            , "          " ]
