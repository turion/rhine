module MyLib where

-- base

import Control.Applicative (Alternative)
-- base-compat

-- transformers

-- text

-- lens
import Control.Lens (Index, IndexedTraversal', IxValue, Ixed (..), Lens', Prism', Traversal', failing, icompose, index, itraversed, re, reindexed, selfIndex, view, (%~), (<.), (^?))
import Control.Monad (guard)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Compat (unzip)
import Data.Functor.Compose (Compose (..))
-- automaton

-- rhine

-- rhine-tree

import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Monoid (Alt (..))
import Data.Proxy (Proxy (..))
import Data.Stream (StreamT (..))
import Data.Stream.Result (mapResultState, unzipResult)
import Data.Text hiding (index)
import FRP.Rhine hiding (readerS, runReaderS, step)
import Types
import Prelude hiding (unzip)

-- FIXME use MonoidAction
addPointer :: NodePointer -> DOMPointer -> DOMPointer
addPointer p = nodePointer %~ (<> p)

lookupDOM :: DOMPointer -> DOM -> Maybe Node
lookupDOM p d = d ^? iPointingDOM . index p

lookupNode :: NodePointer -> Node -> Maybe Node
lookupNode p n = n ^? iPointing . index p

data NodeEvent = NodeEvent
  { nodeEventPointer :: NodePointer,
    nodeEvent :: NodeEvent'
  }

data NodeEvent' = NodeEvent'
  { eventName :: Text,
    payload :: Text
  }

data DOMEvent = DOMEvent
  { eventPointer :: DOMPointer,
    domEvent :: NodeEvent'
  }

addPointerEvent :: NodePointer -> DOMEvent -> DOMEvent
addPointerEvent p DOMEvent {eventPointer, domEvent} = DOMEvent {eventPointer = addPointer p eventPointer, domEvent}

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

data DOMSF td m a b = DOMSF
  { focus :: DOMPointer,
    domSF :: ClSF (StateT Node m) (DOMClock td) a b -- FIXME maybe I really want a SelectClock here? Or rather, the focus defines the select clock
    -- It's a bit weird because I want the tag/type of the selectclock, but I'll never start it
  }

-- FIXME rather a prism
filterFocus :: DOMPointer -> DOMEvent -> Maybe NodeEvent
filterFocus p DOMEvent {eventPointer, domEvent} = flip NodeEvent domEvent <$> subtractDOMPointer eventPointer p

subtractDOMPointer :: DOMPointer -> DOMPointer -> Maybe NodePointer
subtractDOMPointer (DOMPointer pos1 p1) (DOMPointer pos2 p2) = guard (pos1 == pos2) >> subtractNodePointer p1 p2

subtractNodePointer :: NodePointer -> NodePointer -> Maybe NodePointer
subtractNodePointer p Here = Just p
subtractNodePointer Here _ = Nothing
subtractNodePointer (There i1 p1) (There i2 p2) = guard (i1 == i2) >> subtractNodePointer p1 p2

-- FIXME Wrong approach. The clock shouldn't move, the SF should filter
-- FIXME this doesn't move the StateT
-- runDOMSF :: (Monad m) => DOMSF td m a b -> ClSF m (DOMClock td) (DOM, a) (DOM, Maybe b)
-- runDOMSF DOMSF {focus, domSF} = readerS $ (arr (\(ti, (dom, a)) -> (dom, (\tag -> (ti {tag}, a)) <$> filterFocus focus (tag ti))) >>>) $ runStateS $ mapMaybeS $ runReaderS domSF

focusState :: (Functor m) => Lens' s a -> StateT a m b -> StateT s m b
focusState l = StateT . (getCompose .) . l . (Compose .) . runStateT

prismState :: (Applicative m) => Prism' s a -> StateT a m b -> StateT s m (Maybe b)
prismState p = StateT . (\action s -> (s ^? p & traverse action) <&> second (maybe s (view (re p))) . Data.Functor.Compat.unzip) . runStateT

traverseState :: (Applicative m, Alternative f) => Traversal' s a -> StateT a m b -> StateT s m (f b)
traverseState t = fmap getAlt . StateT . (getCompose .) . t . (Compose .) . runStateT . fmap (Alt . pure)

-- FIXME is this actually some kind of lens?
-- Can I generalise to not having an explicit Pointer?
inside :: NodePointer -> DOMSF td m a b -> DOMSF td m a b
inside p DOMSF {focus, domSF} = DOMSF {focus = addPointer p focus, domSF}

-- | Morally an affine traversal
iPointing1 :: IndexedTraversal' Int Node Node
iPointing1 = children . itraversed <. _Child

iPointing :: IndexedTraversal' NodePointer Node Node
iPointing = reindexed (const Here) selfIndex `failing` icompose There iPointing1 iPointing

iPointingDOM1 :: IndexedTraversal' Int DOM Node
iPointingDOM1 = dom . itraversed

iPointingDOM :: IndexedTraversal' DOMPointer DOM Node
iPointingDOM = icompose DOMPointer iPointingDOM1 iPointing

class HasEvent a where
  type Event a :: Type
  type Event a = ()

-- FIXME Maybe At is cleverer
-- FIXME use free category
data EventList a where
  EHere :: (HasEvent a) => Event a -> EventList a
  EThere :: (Ixed a) => Index a -> EventList (IxValue a) -> EventList a

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
    splitEventList (input, EHere event) = Left (input, event)
    splitEventList (input, EThere i eventList) = Right ((input, eventList), i)
