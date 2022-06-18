{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (putChar)
import FRP.Rhine.Terminal
import System.IO
    ( hSetBuffering, stdin, stdout, BufferMode(NoBuffering) )
import System.Terminal
import System.Terminal.Internal
import FRP.Rhine
import GHC.Conc (retry, readTVarIO)
import Control.Concurrent (forkIO)
import Control.Monad (void)

import Test.Hspec
-- import Test.Tasty ()
-- import Test.Tasty.HUnit ( assertEqual )

type KeyClock t = SelectClock (TerminalEventClock t) Char

keyClock :: Terminal t => t -> KeyClock t
keyClock term = SelectClock { mainClock = TerminalEventClock term, select = selectChar }
  where
    -- selectChar :: Terminal t => Tag (TerminalEventClock t) -> Maybe Char
    selectChar = \case
      Right (KeyEvent (CharKey k) _) -> Just k
      _ -> Nothing

defaultSettings :: VirtualTerminalSettings
defaultSettings = VirtualTerminalSettings
    { virtualType         = "xterm"
    , virtualWindowSize   = pure (Size 3 10)
    , virtualEvent        = retry
    , virtualInterrupt    = retry
    }

displayDot :: (Terminal t, MonadScreen m) => ClSF m (KeyClock t) () ()
displayDot = tagS >-> arrMCl (\_ -> do
  putChar '.'
  flush)

testRhine :: Terminal t => t -> Rhine (TerminalT t IO) (KeyClock t) () ()
testRhine term = displayDot @@ keyClock term

main :: IO ()
main = hspec $ do
    describe "rhine-terminal with VirtualTerminal" $ do
      it "replaces virtual inputs by dots" $ do
        t <- withVirtualTerminal defaultSettings $ \t -> do
          void $ liftIO $ forkIO $ runTerminalT (flow $ testRhine t) t
          termCommand t (PutText "ABC")
          termCommand t PutLn
          termCommand t (PutText "123")
          termCommand t PutLn
          termCommand t PutLn
          pure t
        readTVarIO (virtualWindow t) `shouldReturn` expWindow
        where
          expWindow =
              [ "...       "
              , "          "
              , "          " ]
