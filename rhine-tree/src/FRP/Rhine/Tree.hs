module FRP.Rhine.Tree where

-- base

import Control.Applicative (Alternative)
-- base-compat

-- transformers

-- text

-- lens

-- jsaddle

-- rhine-tree

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Lens (Index, IndexedTraversal', IxValue, Ixed (..), Lens', Prism', Traversal', itraversed, re, to, view, (%~), (<.), (^.), (^?), (^@..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State.Strict (StateT (..))
import Control.Monad.Trans.State.Strict qualified as StateT
import Data.Align (Semialign (..))
import Data.Automaton.Trans.Reader (readerS, runReaderS)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Compat (unzip)
import Data.Functor.Compose (Compose (..))
import Data.Kind (Type)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (Alt (..))
import Data.Proxy (Proxy (..))
import Data.Stream (StreamT (..))
import Data.Stream.Result (mapResultState, unzipResult)
import Data.Text hiding (index, length)
import Data.Text qualified as T hiding (length)
import Data.These (these)
import FRP.Rhine hiding (readerS, runReaderS, step)
import FRP.Rhine.Tree.Types
import Language.Javascript.JSaddle (MonadJSM (..), fun, js, jsg, jss, syncPoint, valToNumber)
import Language.Javascript.JSaddle.Types (JSM)
import Prelude hiding (unzip)

default (Text)

class MonadDOM td m where
  waitDOMEvent :: proxy td -> m DOMEvent
  currentTime :: m td

data DOMClock td = DOMClock

instance (MonadDOM td m, TimeDomain td, Monad m) => Clock m (DOMClock td) where
  type Time (DOMClock td) = td
  type Tag (DOMClock td) = DOMEvent
  initClock DOMClock = do
    initTime <- currentTime
    return (constM currentTime &&& constM (waitDOMEvent (Proxy @td)), initTime)

focusState :: (Functor m) => Lens' s a -> StateT a m b -> StateT s m b
focusState l = StateT . (getCompose .) . l . (Compose .) . runStateT

prismState :: (Applicative m) => Prism' s a -> StateT a m b -> StateT s m (Maybe b)
prismState p = StateT . (\action s -> (s ^? p & traverse action) <&> second (maybe s (view (re p))) . Data.Functor.Compat.unzip) . runStateT

traverseState :: (Applicative m, Alternative f) => Traversal' s a -> StateT a m b -> StateT s m (f b)
traverseState t = fmap getAlt . StateT . (getCompose .) . t . (Compose .) . runStateT . fmap (Alt . pure)

-- | Morally an affine traversal
iPointing1 :: IndexedTraversal' Int Node Node
iPointing1 = children . itraversed <. _Child

iPointingDOM1 :: IndexedTraversal' Int DOM Node
iPointingDOM1 = dom . itraversed

class HasEvent a where
  type Event a :: Type
  type Event a = ()

-- FIXME Maybe At is cleverer
-- FIXME use free category
data IndexList c t a b where
  Here :: (c a) => t a -> IndexList c t a a
  There :: (Ixed a) => Index a -> IndexList c t (IxValue a) b -> IndexList c t a b

-- FIXME Stupid workaround because of type families. Maybe we can have an associated data family?
data AnEvent a = AnEvent (Event a)

type EventList = IndexList HasEvent AnEvent

-- FIXME If I had lenses into the inner structure I'd get away with output instead of Maybe output
-- FIXME can we use FilterAutomaton
-- FIXME it mihgt be cleverer to put the Index in a Reader, or even supply a custom asking function
indexAutomaton1 :: (Ixed a, Monad m) => Automaton (StateT (IxValue a) m) input output -> Automaton (StateT a m) (input, Index a) (Maybe output)
indexAutomaton1 = handleAutomaton $ \StreamT {state, step} ->
  StreamT
    { state,
      step = \s -> ReaderT $ \(input, i) ->
        let transition = step s & flip runReaderT input
            maybeStep = traverseState (ix i) transition
         in maybeStep <&> unzipResult <&> mapResultState (fromMaybe s)
    }

-- FIXME test for nested indices
indexAutomaton ::
  forall a b m output input.
  (Ixed a, Monad m) =>
  Automaton (StateT a m) (input, AnEvent a) (Maybe output) ->
  Automaton (StateT (IxValue a) m) (input, EventList (IxValue a) b) output ->
  Automaton (StateT a m) (input, EventList a b) (Maybe output)
indexAutomaton eHere eThere = arr splitIndexList >>> eHere ||| indexAutomaton1 eThere
  where
    -- Need this workaround because GADTs can't be matched in Arrow notation as of 9.10
    splitIndexList :: (input, EventList a b) -> Either (input, AnEvent a) ((input, EventList (IxValue a) b), Index a)
    splitIndexList (input, Here event) = Left (input, event)
    splitIndexList (input, There i eventList) = Right ((input, eventList), i)

data SomeEvent root = forall a. SomeEvent {_someEvent :: EventList root a}

type NodeEvent = SomeEvent Node

type DOMEvent = SomeEvent DOM

class Render a where
  render :: a -> Text

instance Render Attr where
  render Attr {_attrName, _value} = _attrName <> "=\"" <> _value <> "\""

instance Render [Attr] where
  render = T.unwords . fmap render

instance Render Content where
  render (ContentText text) = text
  render (Child node) = render node

instance Render [Content] where
  render = T.unlines . fmap render

instance Render Node where
  render :: Node -> Text
  render Node {_name, _attrs, _children} =
    T.unlines
      [ T.concat
          [ "<",
            _name,
            " ",
            render _attrs,
            ">"
          ],
        render _children,
        T.concat
          [ "</",
            _name,
            ">"
          ]
      ]

instance Render DOM where
  render DOM {_dom} =
    T.unlines $
      "<!doctype html>"
        : (render <$> _dom)

data Edit a = Add a | Delete | Put a

diff0 :: (Eq a) => a -> a -> Maybe (Edit a)
diff0 a1 a2
  | a1 == a2 = Nothing
  | otherwise = Just $ Put a2

-- FIXME Or use SemialignWithIndex
diff :: (Semialign f, Eq a) => (forall x. IndexedTraversal' i (f x) x) -> f a -> f a -> [(i, Edit a)]
diff t fa1 fa2 = align fa1 fa2 ^@.. t <. to (these (pure . const Delete) (pure . Add) diff0) <&> (\(i, me) -> (i,) <$> me) & catMaybes

data JSMEvent = JSMEvent
  { clientX :: Double,
    clientY :: Double
  }

data JSMClock = JSMClock {events :: MVar JSMEvent}

instance GetClockProxy JSMClock

instance (MonadJSM m) => Clock m JSMClock where
  type Time JSMClock = () -- FIXME Use nextAnimationFrame maybe for continuous things?
  type Tag JSMClock = JSMEvent
  initClock JSMClock {events} = return (constM $ ((),) <$> (liftIO (takeMVar events)), ())

createJSMClock :: JSM JSMClock
createJSMClock = do
  events <- liftIO newEmptyMVar
  doc <- jsg ("document" :: Text)
  doc
    ^. jss
      ("onclick" :: Text)
      ( fun $ \_ _ [e] -> do
          clientX <- e ^. js ("clientX" :: Text) >>= valToNumber
          clientY <- e ^. js ("clientY" :: Text) >>= valToNumber
          liftIO $ print clientX
          liftIO $ putMVar events JSMEvent {clientX, clientY}
          syncPoint
      )
  return JSMClock {events}

-- FIXME Next iteration: Cache DOM and only update diff
runStateTDOM :: StateT DOM JSM a -> JSM a
runStateTDOM action = do
  (a, dom_) <- runStateT action mempty
  doc <- jsg ("document" :: Text)
  doc ^. js ("body" :: Text) ^. jss ("innerHTML" :: Text) (render dom_)
  syncPoint -- FIXME needed?
  return a

-- FIXME generalise
type JSMSF node a b = ClSF (StateT node JSM) JSMClock a b

flowJSM :: JSMSF DOM () () -> JSMClock -> JSM ()
flowJSM sf cl = runStateTDOM $ flow $ sf @@ cl

stateS :: (Monad m) => (a -> s -> (b, s)) -> ClSF (StateT s m) cl a b
stateS f = arrMCl $ StateT.state . f

appendS :: (Monoid s, Monad m) => s -> ClSF (StateT s m) cl a ()
appendS s = constMCl $ StateT.modify (<> s)

jsmSF ::
  forall a output input.
  ( Ixed a,
  HasEvent a,
    Event a ~ JSMEvent -- FIXME get rid of that constraint
  ) =>
  JSMSF a input (Maybe output) ->
  JSMSF (IxValue a) input output ->
  JSMSF a input (Maybe output)
jsmSF here there =
  readerS $
    -- FIXME More general routing by getting the evnt structure from the tick
    arr (\(ti, input) -> ((ti, input), Here $ AnEvent $ tag ti))
      >>> indexAutomaton
        (arr (\((ti, input), _) -> (ti, input)) >>> runReaderS here)
        (arr (\((ti, input), _) -> (ti, input)) >>> runReaderS there)

class (Ixed a) => AppendChild a where
  -- | Law:
  -- let (a', i) = appendChild v a in a' ^@? ix i == Just v
  appendChild :: IxValue a -> a -> (Index a, a)

instance AppendChild DOM where
  -- FIXME This is super inefficient, should use a vector or a Seq
  appendChild node dom_ = ( dom_ ^. dom . to length, dom_ & dom %~ (++ [node]))

instance AppendChild Node where
  -- FIXME This is super inefficient, should use a vector or a Seq
  appendChild child parent = (parent ^. children . to length, parent & children %~ (++ [Child child]))

class Register m a where
  register :: IndexList c t root a -> a -> m ()

permanent :: (Event node ~ JSMEvent, Ixed node, HasEvent node, AppendChild node) => IxValue node -> JSMSF node a ()
-- permanent v = jsmSF (arr (const Nothing)) (constMCl (StateT.put v)) >>> arr (const ())
permanent v = constMCl $ StateT.state (appendChild v) <&> const ()
