{-# LANGUAGE ApplicativeDo #-}
import Data.Functor (void)
import Data.Text (Text)
import FRP.Rhine.Tree
import FRP.Rhine.Tree.Types (Content (ContentText), DOM (..), Node (..))
import Language.Javascript.JSaddle
  ( JSM,
  )
import qualified Data.Text as T
import FRP.Rhine (count)

default (Text)

main :: JSM ()
main = do
  clock <- createJSMClock
  logJS "created"
  flowJSM mainClSF clock

mainClSF :: JSMSF DOM () ()
-- mainClSF = do
--   void $ permanent'' $ Node ("p" :: Text) [] [ContentText ("I bims" :: Text)]
--   void $ permanent'' $ Node ("p" :: Text) [] [ContentText ("I bims hier" :: Text)]
-- mainClSF = (void $ permanent'' $ Node ("p" :: Text) [] [ContentText ("I bims" :: Text)]) *>
--   (void $ permanent'' $ Node ("p" :: Text) [] [ContentText ("I bims hier" :: Text)]) -- FIXME y do notaton no work?
mainClSF = void $ proc () -> do
  permanent'' $ Node ("p" :: Text) [] [ContentText ("I bims" :: Text)] -< ()
  (_, i) <- permanent'' $ Node ("div" :: Text) [] [ContentText ("I bims hier" :: Text)] -< ()
  n <- count -< ()
  dynamic (Node "div" [] []) (varying $ Node "p" [] . pure . ContentText) -< T.pack $ show (i, n :: Integer)
