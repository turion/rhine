{-# LANGUAGE TemplateHaskell #-}
module MyLib where

-- base-compat
import Data.List.Compat ((!?))
import Data.Proxy (Proxy (..))

-- transformers
import Control.Monad.Trans.State.Strict (StateT (..))

-- text
import Data.Text

-- automaton

import Data.Automaton.Trans.Reader (readerS, runReaderS)
import Data.Automaton.Trans.State (runStateS)

-- rhine
import FRP.Rhine hiding (readerS, runReaderS)
import Control.Monad (guard)
import Data.Function ((&))
import Control.Lens (Prism', IndexedTraversal', Indexable (..), makeLenses)

data Fix f = Fix (f (Fix f))

newtype DOM = DOM [Node]

data Node = Node
  { name :: Text
  , attrs :: [Attr]
  , children :: [Content]
  }


data Content = ContentText Text | Child Node
data Attr = Attr
  { attrName :: Text
  , value :: Text
  }

data DOMPointer = DOMPointer
  { nodePosition :: Int -- FIXME no safety this index is in bounds
  , nodePointer :: NodePointer
  }

-- FIXME this is morally [Int]
data NodePointer = Here | There Int NodePointer

makeLenses ''Attr
makeLenses ''Node


instance Semigroup NodePointer where
  Here <> p = p
  There i p1 <> p2 = There i $ p1 <> p2

-- FIXME use MonoidAction
addPointer :: NodePointer -> DOMPointer -> DOMPointer
addPointer p DOMPointer {nodePosition, nodePointer} = DOMPointer {nodePosition, nodePointer = nodePointer <> p}

instance Monoid NodePointer where
  mempty = Here

lookupDOM :: DOMPointer -> DOM -> Maybe Node
lookupDOM DOMPointer {nodePosition, nodePointer} (DOM nodes) = nodes !? nodePosition >>= lookupNode nodePointer

lookupNode :: NodePointer -> Node -> Maybe Node
lookupNode Here node = Just node
lookupNode (There childIndex nodePointer) Node {children} = children !? childIndex >>= contentToNode >>= lookupNode nodePointer
  where
    contentToNode (Child node) = Just node
    contentToNode _ = Nothing

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
  , domSF :: ClSF (StateT Node m) (SelectClock (DOMClock td) NodeEvent) a b -- FIXME maybe I really want a SelectClock here? Or rather, the focus defines the select clock
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

-- FIXME this doesn't move the StateT
runDOMSF :: (Monad m) => DOMSF td m a b -> ClSF m (DOMClock td) (DOM, a) (DOM, Maybe b)
runDOMSF DOMSF {focus, domSF} = readerS $ (arr (\(ti, (dom, a)) -> (dom, (\tag -> (ti {tag}, a)) <$> filterFocus focus (tag ti))) >>>) $ runStateS $ mapMaybeS $ runReaderS domSF

-- FIXME Don't get riled up here. We really need a prism from NodePointer
moveStateDeeper :: NodePointer -> StateT Node m a -> StateT Node m a
moveStateDeeper p sma = StateT $ \s -> runStateT sma _ & _

pointing :: NodePointer -> Prism' Node Node
pointing Here = id
-- FIXME derive lenses and implement this
pointing (There i p) = _ . pointing p

-- FIXME is this actually some kind of lens?
-- Can I generalise to not having an explicit Pointer?
inside :: NodePointer -> DOMSF td m a b -> DOMSF td m a b
inside p DOMSF {focus, domSF} = DOMSF {focus = addPointer p focus, domSF}

-- | Morally an affine traversal
iPointing :: IndexedTraversal' NodePointer Node Node
iPointing handler Node {name, attrs, children} = let f = indexed handler in _

iPointingDOM :: IndexedTraversal' DOMPointer DOM Node
iPointingDOM = _
