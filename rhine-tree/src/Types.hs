{-# LANGUAGE TemplateHaskell #-}
module Types where

-- text
import Data.Text

-- lens
import Control.Lens (makeLenses, makePrisms)

data Fix f = Fix {_fix :: f (Fix f)}

makeLenses ''Fix

newtype DOM = DOM {_dom :: [Node]}

data Node = Node
  { _name :: Text
  , _attrs :: [Attr]
  , _children :: [Content]
  }


data Content = ContentText Text | Child Node
data Attr = Attr
  { _attrName :: Text
  , _value :: Text
  }

data DOMPointer = DOMPointer
  { _nodePosition :: Int -- FIXME no safety this index is in bounds
  , _nodePointer :: NodePointer
  }
  deriving Eq

-- FIXME this is morally [Int]
data NodePointer = Here | There Int NodePointer
  deriving Eq

makeLenses ''Attr
makeLenses ''Node
makeLenses ''DOM
makeLenses ''DOMPointer
makePrisms ''NodePointer
makePrisms ''Content

instance Semigroup NodePointer where
  Here <> p = p
  There i p1 <> p2 = There i $ p1 <> p2

instance Monoid NodePointer where
  mempty = Here
