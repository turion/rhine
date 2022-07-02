{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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

type KeyClock t = SelectClock (TerminalEventClock t) Char

keyClock :: Terminal t => t -> KeyClock t
keyClock term = SelectClock { mainClock = TerminalEventClock term, select = selectChar }
  where
    -- selectChar :: Terminal t => Tag (TerminalEventClock t) -> Maybe Char
    selectChar = \case
      Right (KeyEvent (CharKey k) _) -> Just k
      _ -> Nothing

defaultSettings :: TQueue Event -> VirtualTerminalSettings
defaultSettings eventQueue = VirtualTerminalSettings
    { virtualType         = "xterm"
    , virtualWindowSize   = pure (Size 3 10)
    , virtualEvent        = readTQueue eventQueue
    , virtualInterrupt    = retry
    }

displayDot :: (Terminal t, MonadScreen m) => ClSF m (KeyClock t) () ()
displayDot = tagS >-> arrMCl (\_ -> do
  putChar '.'
  flush)

testRhine :: Terminal t => t -> Rhine (TerminalT t IO) (KeyClock t) () ()
testRhine term = displayDot @@ keyClock term

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
          void $ liftIO $ forkIO $ runTerminalT (flow $ testRhine t) t
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
