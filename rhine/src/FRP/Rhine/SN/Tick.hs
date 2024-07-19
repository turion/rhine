{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module FRP.Rhine.SN.Tick where

-- sop-core
import Data.SOP (All, NP (..), NS (..), SListI, hmap, SList (SNil, SCons), sList, HCollapse (hcollapse), K (K))

-- rhine

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:) (..), (:~~:))
import FRP.Rhine.Clock
import Data.SOP.NP (liftA_NP, map_NP)
import Data.Maybe (listToMaybe)
import Data.Function ((&))
import Data.Foldable (Foldable(fold))
import Data.Monoid (First(..))

class HasTimeDomain td cl

instance (Time cl ~ td) => HasTimeDomain td cl

newtype Tick cls = Tick {getTick :: NS TimeInfo cls}

-- This must exist elsewhere?? Or maybe contribute to sop-core?
type Map :: (Type -> Type) -> [Type] -> [Type]
type family Map f ts = ret | ret -> ts where
  Map f '[] = '[]
  Map f (t ': ts) = f t ': Map f ts

-- retick :: All (HasTimeDomain time) cls => Proxy time -> Proxy cls -> Proxy m1 -> Proxy m2 -> Tick (Map (HoistClock m1 m2) cls) -> Tick cls
-- retick _ _ _ _ Tick {getTick = Z ti} = Tick $ Z $ TimeInfo {}
-- retick _ _ _ _ Tick {getTick = S x} = Tick $ _


mapNS :: forall cls f t g . SListI cls => (forall cl. f (t cl) -> g cl) -> NS f (Map t cls) -> NS g cls
mapNS f = case sList @cls of
  SNil -> \case
  SCons -> \case
    Z x -> Z $ f x
    S x -> S $ mapNS f x

retick :: (SListI cls) => (forall cl. TimeInfo (t cl) -> TimeInfo cl) -> Tick (Map t cls) -> Tick cls
retick f = Tick . mapNS f . getTick


{-
class MapLemma cls where
  mapLemma :: Map t cls :~: (t cl ': Map t cls') -> cls :~: (cl ': cls')
  mapLemma' :: Map t (cl ': cls) :~: t cl ': Map t cls

class InjectiveMap cls1 cls2 where
  injective :: Proxy t -> Map t cls1 :~: Map t cls2 -> cls1 :~: cls2

instance InjectiveMap '[] '[] where
  injective _ Refl = Refl

type family InjectiveMap'' t cls1 cls2 where
  InjectiveMap'' t cls1 cls2 = (Map t cls1 ~ Map t cls1) => (cls1 ~ cls2)

class (Map t cls1 ~ Map t cls2) => InjectiveMap' t cls1 cls2 where

instance (Map t cls1 ~ Map t cls2) => InjectiveMap' t cls1 cls2

--   injective _ Refl = Refl

instance MapLemma '[] where
  mapLemma x = _
  mapLemma' = Refl

instance MapLemma cls => MapLemma (cl ': cls) where
  mapLemma p@Refl = consLemma $ mapLemma _
  mapLemma' = Refl

consLemma :: cls1 :~: cls2 -> cl ': cls1 :~: cl ': cls2
consLemma Refl = Refl
-- mapLemma Refl = Refl
-}

-- FIXME look up how something like this is done properly
-- type family HasClock cl (cls :: [Type]) :: Constraint where
--   HasClock cl (cl ': cls) = ()
--   HasClock cl1 (cl2 ': cls) = HasClock cl1 cls

-- FIXME rewrite with sop-core?
-- FIXME rewrite with prisms?
class HasClock cl cls where
  position :: Position cl cls

instance HasClock cl (cl ': cls) where
  position = Z Refl

instance {-# OVERLAPPABLE #-} (HasClock cl cls) => HasClock cl (cl' ': cls) where
  position = S position

inject :: forall cl cls. (HasClock cl cls) => Proxy cl -> TimeInfo cl -> Tick cls
inject _ = Tick . injectPosition (position @cl @cls)

injectPosition :: Position cl cls -> f cl -> NS f cls
injectPosition (Z Refl) ti = Z ti
injectPosition (S pointer) ti = S $ injectPosition pointer ti

project :: forall cl cls. (HasClock cl cls) => Proxy cl -> Tick cls -> Maybe (TimeInfo cl)
project _ = projectPosition (position @cl @cls) . getTick

projectPosition :: Position cl cls -> NS f cls -> Maybe (f cl)
projectPosition (Z Refl) (Z ti) = Just ti
projectPosition (S position) (S tick) = projectPosition position tick
projectPosition _ _ = Nothing

-- type family HasClocksOrdered clA clB (cls :: [Type]) :: Constraint where
--   HasClocksOrdered clA clB (clA ': cls) = HasClock clB cls
--   HasClocksOrdered clA clB (cl ': cls) = HasClocksOrdered clA clB cls

class HasClocksOrdered clA clB cls where
  orderedPositions :: OrderedPositions clA clB cls

instance (HasClock clB cls) => HasClocksOrdered clA clB (clA ': cls) where
  orderedPositions = OPHere position

instance {-# OVERLAPPABLE #-} (HasClocksOrdered clA clB cls) => HasClocksOrdered clA clB (cl ': cls) where
  orderedPositions = OPThere orderedPositions

firstPosition :: OrderedPositions clA clB cls -> Position clA cls
firstPosition (OPHere _) = Z Refl
firstPosition (OPThere positions) = S $ firstPosition positions

secondPosition :: OrderedPositions clA clB cls -> Position clB cls
secondPosition (OPHere pos) = S pos
secondPosition (OPThere positions) = S $ secondPosition positions

newtype PositionIn cls cl = PositionIn {getPositionIn :: Position cl cls}

-- | Whether 'clsSub' is a subsequence of 'cls'
class HasClocks clsSub cls where
  positions :: NP (PositionIn cls) clsSub

instance HasClocks '[] cls where
  positions = Nil

instance (SListI clsSub, HasClocks clsSub cls) => HasClocks (cl ': clsSub) (cl ': cls) where
  positions = PositionIn position :* liftA_NP (PositionIn . S . getPositionIn) positions

instance (SListI clsSub, HasClocks clsSub cls) => HasClocks clsSub (cl ': cls) where
  positions = liftA_NP (PositionIn . S . getPositionIn) positions

data OrderedPositions cl1 cl2 cls where
  OPHere :: Position cl2 cls -> OrderedPositions cl1 cl2 (cl1 ': cls)
  OPThere :: OrderedPositions cl1 cl2 cls -> OrderedPositions cl1 cl2 (cl ': cls)


type Position cl cls = NS ((:~:) cl) cls

injectPositions :: NP (PositionIn cls) clsSub -> NS f clsSub -> NS f cls
injectPositions (PositionIn pos :* _) (Z x) = injectPosition pos x
injectPositions (_ :* pos) (S x) = injectPositions pos x
injectPositions Nil x = case x of

injectTick :: HasClocks clsSub cls => Tick clsSub -> Tick cls
injectTick Tick {getTick} = Tick $ injectPositions positions getTick

projectTick :: HasClocks clsSub cls => Tick cls -> Maybe (Tick clsSub)
projectTick Tick {getTick} = Tick <$> projectPositions positions getTick

projectPositions :: NP (PositionIn cls) clsSub -> NS f cls -> Maybe (NS f clsSub)
projectPositions Nil _ = Nothing
projectPositions (PositionIn (Z Refl) :* _) (Z x) = Just $ Z x
projectPositions (PositionIn (S _) :* _) (Z _) = Nothing
projectPositions (PositionIn (S pos) :* poss) (S x) = projectPositions (PositionIn pos :* poss) x
projectPositions (_ :* poss) (S x) = _
-- projectPositions poss x = poss
--   & _ x
--   & hcollapse
--   & fold
--   & getFirst
