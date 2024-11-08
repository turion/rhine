module MyLib where

-- base-compat
import Data.List.Compat ((!?))
import Data.Proxy (Proxy (..))
import Data.Functor.Compat (unzip)

-- transformers
import Control.Monad.Trans.State.Strict (StateT (..))

-- text
import Data.Text hiding (index)

-- automaton

import Data.Automaton.Trans.Reader (readerS, runReaderS)
import Data.Automaton.Trans.State (runStateS)

-- rhine
import FRP.Rhine hiding (step, readerS, runReaderS)
import Control.Monad (guard)
import Data.Function ((&))
import Control.Lens (Prism', IndexedTraversal', Indexable (..), itraversed, Lens', (^?), view, re, (%~), (<.), failing, selfIndex, reindexed, icompose, index, Ixed, IxValue, Index)
import Data.Functor.Compose (Compose(..))
import Data.Functor ((<&>))

import Types
import Data.Maybe (fromMaybe)
import Data.Stream (StreamT(..))

-- FIXME use MonoidAction
addPointer :: NodePointer -> DOMPointer -> DOMPointer
addPointer p = nodePointer %~ (<> p)

lookupDOM :: DOMPointer -> DOM -> Maybe Node
lookupDOM p d = d ^? iPointingDOM . index p

lookupNode :: NodePointer -> Node -> Maybe Node
lookupNode p n = n ^? iPointing . index p

data NodeEvent = NodeEvent
  { nodeEventPointer :: NodePointer
  , nodeEvent :: Event
  }

data Event = Event
  { eventName :: Text
  , payload :: Text
  }

data DOMEvent = DOMEvent
  { eventPointer :: DOMPointer
  , domEvent :: Event
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
  { focus :: DOMPointer
  , domSF :: ClSF (StateT Node m) (DOMClock td) a b -- FIXME maybe I really want a SelectClock here? Or rather, the focus defines the select clock
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

-- FIXME Don't get riled up here. We really need a prism from NodePointer
moveStateDeeper :: NodePointer -> StateT Node m a -> StateT Node m a
moveStateDeeper p sma = StateT $ \s -> runStateT sma _ & _

focusState :: Functor m => Lens' s a -> StateT a m b -> StateT s m b
focusState l = StateT . (getCompose .) . l . (Compose .) . runStateT

prismState :: Applicative m => Prism' s a -> StateT a m b -> StateT s m (Maybe b)
prismState p = StateT . (\action s -> (s ^? p & traverse action) <&> second (maybe s (view (re p))) . Data.Functor.Compat.unzip) . runStateT

-- FIXME Can generalise from [b] to Traversable t => t b?
traverseState :: IndexedTraversal' i s a -> StateT a m b -> StateT s m [b]
traverseState = _

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

-- FIXME Maybe At is cleverer
-- FIXME use free category
data IndexList a b where
  Id :: IndexList a a
  Cons :: Ixed a => Index a -> IndexList (IxValue a) b -> IndexList a b

-- FIXME can we use FilterAutomaton
indexAutomaton1 :: (Ixed a, Monad m) => Automaton (StateT (IxValue a) m) input output -> Automaton (StateT a m) (input, Index a) [output]
indexAutomaton1 = handleAutomaton $ \StreamT {state, step} -> StreamT
  { state
  , step = _
  }