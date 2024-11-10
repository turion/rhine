module FRP.Rhine.Tree where

-- base

import Control.Applicative (Alternative)
-- base-compat

-- transformers

-- text

-- lens
import Control.Lens (Index, IndexedTraversal', IxValue, Ixed (..), Lens', Prism', Traversal', failing, icompose, index, itraversed, re, reindexed, selfIndex, view, (%~), (<.), (^?), (^@..), to, Optic')
import Control.Monad (guard)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State.Strict (StateT (..))
-- automaton

-- rhine

-- rhine-tree

import Data.Align (Semialign (..))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Compat (unzip)
import Data.Functor.Compose (Compose (..))
import Data.Kind (Type)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid (Alt (..))
import Data.Proxy (Proxy (..))
import Data.Stream (StreamT (..))
import Data.Stream.Result (mapResultState, unzipResult)
import Data.Text hiding (index)
import Data.Text qualified as T
import FRP.Rhine hiding (readerS, runReaderS, step)
import Types
import Prelude hiding (unzip)
import Data.These (these, These)
import Data.Semialign.Indexed (SemialignWithIndex)

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
data EventList a where
  Here :: (HasEvent a) => Event a -> EventList a
  There :: (Ixed a) => Index a -> EventList (IxValue a) -> EventList a

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
  forall a m output input.
  (Ixed a, Monad m) =>
  Automaton (StateT a m) (input, Event a) output ->
  Automaton (StateT (IxValue a) m) (input, EventList (IxValue a)) output ->
  Automaton (StateT a m) (input, EventList a) (Maybe output)
indexAutomaton eHere eThere = arr splitEventList >>> (eHere >>> arr Just) ||| indexAutomaton1 eThere
  where
    -- Need this workaround because GADTs can't be matched in Arrow notation as of 9.10
    splitEventList :: (input, EventList a) -> Either (input, Event a) ((input, EventList (IxValue a)), Index a)
    splitEventList (input, Here event) = Left (input, event)
    splitEventList (input, There i eventList) = Right ((input, eventList), i)

type NodeEvent = EventList Node

type DOMEvent = EventList DOM

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
