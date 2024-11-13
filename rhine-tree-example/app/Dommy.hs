module Dommy where

import FRP.Rhine.Tree

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Lens ((^.))
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Monoid ((<>))
import Language.Javascript.JSaddle (
  askJSM,
  fun,
  global,
  js,
  js1,
  jsg,
  jsg3,
  jss,
  nextAnimationFrame,
  runJSM,
  syncPoint,
  valToNumber,
  valToJSON
 )

import FRP.Rhine
import FRP.Rhine.Tree
import FRP.Rhine.Tree.Types (DOM(..))
import FRP.Rhine.Tree.Types (Node(..), Content (ContentText))

main :: JSM ()
main = do
  clock <- createJSMClock
  flowJSM mainClSF clock

mainClSF :: JSMSF () ()
mainClSF = appendS $ DOM [Node "p" [] [ContentText "Hi"]]
