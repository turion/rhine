{-# LANGUAGE TemplateHaskell #-}
module FRP.Rhine.Tree.Types where

-- text
import Data.Text

-- lens
import Control.Lens (makeLenses, makePrisms, Index, Ixed (..), IxValue)

data Fix f = Fix {_fix :: f (Fix f)}

makeLenses ''Fix

newtype DOM = DOM {_dom :: [Node]}

instance Semigroup DOM where
  DOM dom1 <> DOM dom2 = DOM $ dom1 <> dom2

instance Monoid DOM where
  mempty = DOM mempty

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


makeLenses ''Attr
makeLenses ''Node
makeLenses ''DOM
makePrisms ''Content

type instance Index Node = Int
type instance IxValue Node = Node

instance Ixed Node where
  ix i = children . ix i . _Child

type instance Index DOM = Int
type instance IxValue DOM = Node

instance Ixed DOM where
  ix i = dom . ix i
