{-# LANGUAGE TemplateHaskell #-}
-- text
-- lens
{-# OPTIONS_GHC -Wno-orphans #-}

module FRP.Rhine.Tree.Types where

import Control.Lens (At, Index, IxValue, Ixed (..), Prism', lens, makeLenses, makePrisms, prism)
import Control.Lens.At (At (..))
import Data.Finite (Finite)
import Data.SOP (I, NP)
import Data.Text
import Data.Vector qualified as V
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as S
import GHC.TypeLits (KnownNat)

data Fix f = Fix {_fix :: f (Fix f)}

makeLenses ''Fix

newtype DOM = DOM {_dom :: [Node]}

instance Semigroup DOM where
  DOM dom1 <> DOM dom2 = DOM $ dom1 <> dom2

instance Monoid DOM where
  mempty = DOM mempty

data Node = Node
  { _name :: Text,
    _attrs :: [Attr],
    _children :: V.Vector Content
  }

someVector :: (KnownNat n) => Prism' (V.Vector Content) (S.Vector n Content)
someVector = prism S.fromSized $ \v -> maybe (Left v) Right $ S.toSized v

type instance Index (S.Vector n a) = Finite n

type instance IxValue (S.Vector n (Maybe a)) = a

instance Ixed (S.Vector n (Maybe a))

-- Deep subsumption
{- HLINT at ignore "Eta reduce" -}
instance At (S.Vector n (Maybe a)) where
  at i = S.ix i

data Content = ContentText Text | Child Node

data Attr = Attr
  { _attrName :: Text,
    _value :: Text
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
