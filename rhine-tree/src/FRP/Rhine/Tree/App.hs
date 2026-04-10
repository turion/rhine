-- | The demo rhine-tree application, shared between the WASM and Warp executables.
module FRP.Rhine.Tree.App where

import Data.Functor (void)
import Data.Text (Text)
import Data.Text qualified as T
import FRP.Rhine (count)
import FRP.Rhine.Tree
import FRP.Rhine.Tree.Types (Content (ContentText), Node (..))
import Language.Javascript.JSaddle (JSM)

default (Text)

-- | The top-level JSM entry point.
mainJSM :: JSM ()
mainJSM = do
  clock <- createJSMClock
  logJS "created"
  flowJSM mainClSF clock

mainClSF :: JSMSF DOM () ()
mainClSF = void $ proc () -> do
  permanent'' $ Node ("p" :: Text) [] [ContentText ("I bims" :: Text)] -< ()
  (_, i) <- permanent'' $ Node ("div" :: Text) [] [ContentText ("I bims hier" :: Text)] -< ()
  n <- count -< ()
  dynamic (Node "div" [] []) (varying $ Node "p" [] . pure . ContentText) -< T.pack $ show (i, n :: Integer)
