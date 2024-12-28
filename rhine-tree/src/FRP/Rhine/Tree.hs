{-# LANGUAGE Arrows #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FRP.Rhine.Tree where

-- base

import Control.Applicative (Alternative)
-- base-compat

-- transformers

-- text

-- lens

-- jsaddle

-- rhine-tree

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Lens (ALens', Index, IndexedTraversal', IxValue, Ixed (..), Lens', Prism', Traversal', itraversed, re, to, view, (%~), (<.), (^.), (^?), (^@..))
import Control.Monad (join, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State.Strict (StateT (..))
import Control.Monad.Trans.State.Strict qualified as StateT
import Data.Align (Semialign (..))
import Data.Automaton.Trans.Reader (readerS, runReaderS)
import Data.Automaton.Trans.State (runStateS)
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
import Data.Vector qualified as V
import FRP.Rhine hiding (readerS, runReaderS, step)
import FRP.Rhine.ClSF.State qualified as ClSF
import FRP.Rhine.Tree.Types
import Language.Javascript.JSaddle (MonadJSM (..), fun, js, js1, js2, jsg, jss, syncPoint, valToNumber)
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

instance HasEvent DOM where
  type Event DOM = JSMEvent

-- FIXME Maybe At is cleverer
-- FIXME use free category
data IndexList c t a b where
  Here :: (c a) => t a -> IndexList c t a a
  There :: (Ixed a) => Index a -> IndexList c t (IxValue a) b -> IndexList c t a b

-- Lensing :: AtL a => Index a -> IndexList c t (IxValue a) b -> IndexList c t a b

-- FIXME Stupid workaround because of type families. Maybe we can have an associated data family?
newtype AnEvent a = AnEvent (Event a)

type EventList = IndexList HasEvent AnEvent

lensAutomaton :: (Monad m) => Lens' s a -> Automaton (StateT a m) i o -> Automaton (StateT s m) i o
lensAutomaton l = hoistS $ focusState l

-- FIXME If I had lenses into the inner structure I'd get away with output instead of Maybe output
-- FIXME can we use FilterAutomaton? At least we can use any Alternative
-- FIXME it mihgt be cleverer to put the Index in a Reader, or even supply a custom asking function
indexAutomaton1 :: (Ixed a, Monad m) => Automaton (StateT (IxValue a) m) input output -> Automaton (StateT a m) (input, Index a) (Maybe output)
indexAutomaton1 = handleAutomaton $ \StreamT {state, step} ->
  StreamT
    { state,
      step = \s -> ReaderT $ \(input, i) ->
        let transition = step s & flip runReaderT input
            maybeStep = traverseState (ix i) transition
         in (maybeStep <&> mapResultState (fromMaybe s) . unzipResult)
    }

-- FIXME test for nested indices
indexAutomaton ::
  forall a b m output input.
  (Ixed a, Monad m) =>
  Automaton (StateT a m) (input, AnEvent a) (Maybe output) ->
  Automaton (StateT (IxValue a) m) (input, EventList (IxValue a) b) output ->
  Automaton (StateT a m) (input, EventList a b) (Maybe output)
-- indexAutomaton eHere eThere = arr splitIndexList >>> eHere ||| indexAutomaton1 eThere
indexAutomaton eHere eThere = proc i -> do
  case splitIndexList i of
    Left ia -> eHere -< ia
    Right (ie, Left i) -> indexAutomaton1 eThere -< (ie, i)
  where
    -- Right (ie, Right l) -> lensAutomaton eThere -< _

    -- Need this workaround because GADTs can't be matched in Arrow notation as of 9.10
    splitIndexList :: (input, EventList a b) -> Either (input, AnEvent a) ((input, EventList (IxValue a) b), Either (Index a) (ALens' a (IxValue a)))
    splitIndexList (input, Here event) = Left (input, event)
    splitIndexList (input, There i eventList) = Right ((input, eventList), Left i)

-- splitIndexList (input, Lensing i eventList) = Right ((input, eventList), Right $ atl i)

data SomeEvent root = forall a. SomeEvent {_someEvent :: EventList root a}

someEventHere :: SomeEvent node -> Maybe (Event node)
someEventHere (SomeEvent (Here (AnEvent e))) = Just e
someEventHere (SomeEvent (There _ _)) = Nothing

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

instance Render (V.Vector Content) where
  render = render . V.toList

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
  render DOM {_dom} = T.unlines $ render <$> _dom

-- T.unlines $
--   "<!doctype html>"
--     : (render <$> _dom)

data Edit a = Add a | Delete | Put a

diff0 :: (Eq a) => a -> a -> Maybe (Edit a)
diff0 a1 a2
  | a1 == a2 = Nothing
  | otherwise = Just $ Put a2

-- FIXME Or use SemialignWithIndex
diff :: (Semialign f, Eq a) => (forall x. IndexedTraversal' i (f x) x) -> f a -> f a -> [(i, Edit a)]
diff t fa1 fa2 = align fa1 fa2 ^@.. t <. to (these (pure . const Delete) (pure . Add) diff0) <&> (\(i, me) -> (i,) <$> me) & catMaybes

data JSMEvent
  = OnClick
      { clientX :: Double,
        clientY :: Double
      }
  | DOMContentLoaded
  | RhineStarted

newtype JSMClock (node :: Type) = JSMClock {events :: Chan (SomeEvent node)}

instance GetClockProxy (JSMClock node)

instance (MonadJSM m) => Clock m (JSMClock node) where
  type Time (JSMClock node) = () -- FIXME Use nextAnimationFrame maybe for continuous things?
  type Tag (JSMClock node) = SomeEvent node
  initClock JSMClock {events} = return (constM $ ((),) <$> liftIO (readChan events), ())

createJSMClock :: JSM (JSMClock DOM)
createJSMClock = do
  events <- liftIO newChan
  liftIO $ writeChan events $ SomeEvent $ Here $ AnEvent RhineStarted
  doc <- jsg ("document" :: Text)
  doc
    ^. jss
      ("onclick" :: Text)
      ( fun $ \a b [e] -> do
          clientX <- e ^. js ("clientX" :: Text) >>= valToNumber
          clientY <- e ^. js ("clientY" :: Text) >>= valToNumber
          liftIO $ print clientX
          liftIO $ writeChan events $ SomeEvent $ Here $ AnEvent OnClick {clientX, clientY}
          syncPoint
      )
  doc
    ^. jss
      ("DOMContentLoaded" :: Text)
      ( fun $ \_ _ _ -> do
          liftIO $ putStrLn "load"
          liftIO $ writeChan events $ SomeEvent $ Here $ AnEvent DOMContentLoaded
          syncPoint
      )
  return JSMClock {events}

-- FIXME Next iteration: Cache DOM and only update diff
-- FIXME Also register event listeners when dom nodes are created
runStateTDOM :: StateT DOM JSM a -> JSM a
runStateTDOM action = do
  logJS "starting runStateTDOM"
  (a, dom_) <- runStateT action mempty
  logJS "Calculated:"
  logJS $ render dom_
  doc <- jsg ("document" :: Text)
  doc ^. (js ("body" :: Text) . jss ("innerHTML" :: Text) (render dom_))
  doc
    ^. js ("body" :: Text)
      . js1 ("querySelectorAll" :: Text) ("*" :: Text)
      . js1
        ("forEach" :: Text)
        ( fun $ \_ _ [el] -> do
            el
              ^. js2
                ("addEventListener" :: Text)
                ("click" :: Text)
                ( fun $ \_ _ e -> do
                    logJS "something happened"
                )
            liftIO $ putStrLn "could have"
        )
  logJS "done"
  syncPoint -- FIXME needed?
  return a

runStateTDOMS :: JSMSF DOM a b -> ClSF JSM (JSMClock DOM) a b
runStateTDOMS sf = feedback mempty $ proc (a, dom_) -> do
  constMCl $ logJS "starting runStateTDOM" -< ()
  (dom', b) <- ClSF.runStateS sf -< (dom_, a)
  constMCl $ logJS "Calculated:" -< ()
  arrMCl logJS -< render dom'
  doc <- constMCl $ jsg ("document" :: Text) -< ()
  arrMCl (\(t, doc) -> doc ^. (js ("body" :: Text) . jss ("innerHTML" :: Text) t)) -< (render dom', doc)
  constMCl syncPoint -< ()
  constMCl $ logJS "syncPoint reached" -< ()
  returnA -< (b, dom')

-- type TreeSF m cl root node i o = Tag cl ~ SomeEvent root => ClSF (StateT node m) cl i o
type TreeSF' m cl node i o = ClSF (StateT node m) (cl node) i o

{-
pushTreeSF :: forall a m cl root i o . (Ixed a, Monad m) => TreeSF m cl root (IxValue a) i o -> TreeSF m cl root a i (Maybe o)
-- FIXME Use filter automaton at some point?
pushTreeSF sf =  readerS (arr filterTi >>> mapMaybeS (indexAutomaton1 (runReaderS sf)) >>> arr join)
  where
    filterTi :: (TimeInfo cl, i) -> Maybe ((TimeInfo cl, i), Index a)
    filterTi (ti@TimeInfo {tag = SomeEvent (There ix el)}, i) = _
-- pushTreeSF sf =  (readerS $ (mapMaybeS (arr _ >>> indexAutomaton1 (runReaderS sf))) >>> arr _) >>> arr _
-}

pushTreeSF ::
  forall m a cl i o.
  (Monad m, Ixed a, Tag (cl a) ~ SomeEvent a, Tag (cl (IxValue a)) ~ SomeEvent (IxValue a), Time (cl a) ~ Time (cl (IxValue a))) =>
  TreeSF' m cl (IxValue a) i o ->
  TreeSF' m cl a i (Maybe o)
-- FIXME Use filter automaton at some point?
pushTreeSF sf = readerS $ arr filterTi >>> mapMaybeS (indexAutomaton1 (runReaderS sf)) >>> arr join
  where
    filterTi :: (TimeInfo (cl a), i) -> Maybe ((TimeInfo (cl (IxValue a)), i), Index a)
    filterTi (ti@TimeInfo {tag = SomeEvent (There idx el)}, i) = Just ((retag (const (SomeEvent el)) ti, i), idx)
    filterTi (TimeInfo {tag = SomeEvent (Here _)}, _) = Nothing

pushTreeSF' ::
  forall m a cl i o.
  (Monad m, Ixed a, Tag (cl a) ~ SomeEvent a, Tag (cl (IxValue a)) ~ SomeEvent (IxValue a), Time (cl a) ~ Time (cl (IxValue a))) =>
  TreeSF' m cl (IxValue a) (Index a, i) o ->
  TreeSF' m cl a i (Maybe o)
-- FIXME Use filter automaton at some point?
pushTreeSF' sf = readerS $ arr filterTi >>> mapMaybeS (indexAutomaton1 (runReaderS sf)) >>> arr join
  where
    filterTi :: (TimeInfo (cl a), i) -> Maybe ((TimeInfo (cl (IxValue a)), (Index a, i)), Index a)
    filterTi (ti@TimeInfo {tag = SomeEvent (There idx el)}, i) = Just ((retag (const (SomeEvent el)) ti, (idx, i)), idx)
    filterTi (TimeInfo {tag = SomeEvent (Here _)}, _) = Nothing

-- -- FIXME I want this in pushTreeSF somehow
-- onlyAt :: (Monad m, Tag (cl (IxValue node)) ~ SomeEvent (IxValue node)) => proxy node -> TreeSF' m cl (IxValue node) (Index node, a) (Maybe a)
-- onlyAt _ = proc (index, a) -> do
--   tag <- tagS -< ()
--   returnA -< case tag of
--     SomeEvent (Here (AnEvent e)) -> _
--     _ -> Nothing

-- FIXME generalise
-- type JSMSF node a b = ClSF (StateT node JSM) JSMClock a b
type JSMSF node a b = TreeSF' JSM JSMClock node a b

flowJSM :: JSMSF DOM () () -> JSMClock DOM -> JSM ()
flowJSM sf cl = flow $ runStateTDOMS sf @@ cl

stateS :: (Monad m) => (a -> s -> (b, s)) -> ClSF (StateT s m) cl a b
stateS f = arrMCl $ StateT.state . f

appendS :: (Monoid s, Monad m) => s -> ClSF (StateT s m) cl a ()
appendS s = constMCl $ StateT.modify (<> s)

{-
jsmSF ::
  forall a output input.
  ( Ixed a,
    HasEvent a,
    Event a ~ JSMEvent -- FIXME get rid of that constraint
  ) =>
  JSMSF a input (Maybe output) ->
  JSMSF (IxValue a) input output ->
  JSMSF a input (Maybe output)
    -- FIXME More general routing by getting the evnt structure from the tick
jsmSF here there =
  readerS $
    arr (\(ti, input) -> ((ti, input), Here $ AnEvent $ tag ti))
      >>> indexAutomaton
        (arr (\((ti, input), _) -> (ti, input)) >>> runReaderS here)
        (arr (\((ti, input), _) -> (ti, input)) >>> runReaderS there)
-}

-- FIXME Naming
-- FIXME does this exist already
class (Ixed a) => AtL a where
  atl :: Index a -> Lens' a (IxValue a)

class (Ixed a) => AppendChild a where
  -- | Law:
  -- let (a', i) = appendChild v a in a' ^@? ix i == Just v
  appendChild :: IxValue a -> a -> (Index a, a)

instance AppendChild DOM where
  -- FIXME This is super inefficient, should use a vector or a Seq
  appendChild node dom_ = (dom_ ^. dom . to length, dom_ & dom %~ (++ [node]))

instance AppendChild Node where
  -- FIXME This is super inefficient, should use a vector or a Seq
  appendChild child parent = (parent ^. children . to length, parent & children %~ (`V.snoc` Child child))

class Register m a where
  register :: IndexList c t root a -> a -> m ()

permanent :: (AppendChild node) => IxValue node -> JSMSF node a ()
-- permanent v = jsmSF (arr (const Nothing)) (constMCl (StateT.put v)) >>> arr (const ())
permanent v = constMCl $ void (StateT.state (appendChild v))

permanent' :: (Monad m) => node -> TreeSF' m cl node a ()
permanent' = constMCl . StateT.put

varying :: (Monad m) => (a -> node) -> TreeSF' m cl node a ()
varying f = arrMCl $ StateT.put . f

eventHere :: (Tag (cl node) ~ SomeEvent node, Monad m) => TreeSF' m cl node a (Maybe (Event node))
eventHere = tagS >>> arr someEventHere

permanent'' :: (AppendChild node) => IxValue node -> JSMSF node a (Maybe (Event (IxValue node)), Index node)
permanent'' v = feedback Nothing $ proc (_, iMaybe) -> do
  i' <- case iMaybe of
    Nothing -> do
      i <- constMCl $ StateT.state $ appendChild v -< ()
      returnA -< i
    Just i -> do
      returnA -< i
  mEvent <- pushTreeSF eventHere -< ()
  returnA -< ((join mEvent, i'), Just i')

dynamic :: (AppendChild node, Eq (Index node)) => IxValue node -> JSMSF (IxValue node) a b -> JSMSF node a (Maybe b)
dynamic v sf = proc a -> do
  (_, i) <- permanent'' v -< ()
  constMCl (lift $ logJS "dyńamic") -< ()
  dynamicAt sf -< (i, a) -- FIXME But this doesn't start because there is no event going to it.
  -- It's time to do dom diffing and attaching events

dynamicAt :: (AppendChild node, Eq (Index node)) => JSMSF (IxValue node) a b -> JSMSF node (Index node, a) (Maybe b)
dynamicAt sf = arr join <<< pushTreeSF' sf'
  where
    sf' = proc (i1, (i2, a')) -> do
      if i1 == i2
        then do
          constMCl (lift $ logJS "equal") -< ()
          arr Just <<< sf -< a'
        else do
          constMCl (lift $ logJS "different") -< ()
          returnA -< Nothing

-- modal :: TreeSF' m cl (IxValue node) (i, a) o -> TreeSF' m cl node (i, Maybe a) o
-- modal sf = _

logJS :: Text -> JSM ()
logJS msg = do
  c <- jsg ("console" :: Text)
  void $ c ^. js1 ("log" :: Text) msg
