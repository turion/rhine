{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Data.MonadicStreamFunction.RandomMSF where

import Control.Monad.Bayes.Class

import qualified Data.Vector as V

data Fun a b where
  Id :: Fun a a
  Const :: b -> Fun a b
  Random :: Dist a b -> Fun a b
  Add :: Num a => Fun (a, a) a
  IfThenElse :: Fun a Bool -> Fun a b -> Fun a b -> Fun a b -- FIXME is this enough to invent all of ArrowChoice
  Compare :: Ord b => Fun (b, b) Ordering
  Eq :: Eq b => Fun (b, b) Bool
  Not :: Fun Bool Bool
  And :: Fun (Bool, Bool) Bool
  Fst :: Fun (a, b) a
  Snd :: Fun (a, b) b
  Compose :: Fun a b -> Fun b c -> Fun a c
  Par :: Fun a b -> Fun a c -> Fun a (b, c)

interpretFun :: MonadDistribution m => Fun a b -> a -> m b
interpretFun Id a = return a
interpretFun (Const b) _a = return b
interpretFun (Random dist) a = interpretDist dist a
interpretFun Add (a1, a2) = return $ a1 + a2
interpretFun (IfThenElse if_ then_ else_) a = do
  bool <- interpretFun if_ a
  if bool
    then interpretFun then_ a
    else interpretFun else_ a
interpretFun Compare (b1, b2) = return $ compare b1 b2
interpretFun Eq (b1, b2) = return $ b1 == b2
interpretFun Not b = return $ not b
interpretFun And (b1, b2) = return $ b1 && b2
interpretFun Fst (a, _b) = return a
interpretFun Snd (_a, b) = return b
interpretFun (Compose f g) a = do
  b <- interpretFun f a
  interpretFun g b
interpretFun (Par f g) a = do
  b <- interpretFun f a
  c <- interpretFun g a
  return (b, c)

instance Num b => Num (Fun a b) where
  fromInteger = Const . fromInteger
  f + g = Compose (Par f g) Add
  (*) = error "not implemented"
  abs = error "not implemented"
  signum = error "not implemented"
  (-) = error "not implemented"

-- FIXME overload lambda for Fun?

choice :: MonadDistribution m => [(Double, m a)] -> m a
choice choices = do
  i <- categorical $ V.fromList $ fst <$> choices
  chooseEach <- mapM snd choices
  return $ chooseEach !! i

instance RandomFun Double Double where
  randomFun = choice
    [(0.3, return Id)
    ,(0.2, Const <$> normal 0 1)
    , (0.5, compositions)
    ]

compositions :: forall a m c .
  ( MonadDistribution m
  , RandomFun a Double, RandomFun Double c
  , RandomFun a Bool, RandomFun Bool c
  , RandomFun a (Double, Double), RandomFun (Double, Double) c
  , RandomFun a (Double, Bool), RandomFun (Double, Bool) c
  , RandomFun a (Bool, Double), RandomFun (Bool, Double) c
  , RandomFun a (Bool, Bool), RandomFun (Bool, Bool) c
  , RandomFun a Ordering, RandomFun Ordering c
  , RandomFun a c
  , RandomConst c
  ) =>
  m (Fun a c)
compositions = choice
  [ (0.2, Compose <$> randomFun @a @Double <*> randomFun)
  , (0.2, Compose <$> randomFun @a @Bool <*> randomFun)
  , (0.2, Compose <$> randomFun @a @(Double, Double) <*> randomFun)
  , (0.2, Compose <$> randomFun @a @(Double, Bool) <*> randomFun)
  , (0.2, Compose <$> randomFun @a @(Bool, Bool) <*> randomFun)
  , (0.2, Compose <$> randomFun @a @Ordering <*> randomFun)
  -- , (0.3, Compose <$> randomFun @a @(Double, a) <*> randomFun) -- FIXME need these kinds of recursion
  , (0.2, IfThenElse <$> randomFun <*> randomFun <*> randomFun)
  , (0.1, randomConstFun)
  ]

pars ::
  ( MonadDistribution m
  , RandomFun a b
  , RandomFun a c
  ) =>
  m (Fun a (b, c))
pars = Par <$> randomFun <*> randomFun

instance RandomFun Bool Bool where
  randomFun = choice
    [ (0.4, return Id)
    , (0.3, return Not)
    , (0.3, compositions)
    ]

instance RandomFun Double Bool where
  randomFun = compositions

instance RandomFun Bool Double where
  randomFun = choice
    [(0.5, Const <$> normal 0 1)
    ,(0.5, (\x y -> IfThenElse Id (Const x) (Const y)) <$> normal 0 1 <*> normal 0 1)
    ]

instance RandomFun (Double, Double) Double where
  randomFun = choice
    [ (0.3, return Add)
    , (0.2, return Fst)
    , (0.2, return Snd)
    , (0.3, compositions)
    ]

-- FIXME All of the following also need constants
instance RandomFun (Double, Bool) Double where
  randomFun = choice
    [ (0.5, return Fst)
    , (0.5, compositions)
    ]
instance RandomFun (Double, Bool) Bool where
  randomFun = choice
    [ (0.5, return Snd)
    , (0.5, compositions)
    ]
instance RandomFun (Bool, Double) Double where
  randomFun = choice
    [ (0.5, return Snd)
    , (0.5, compositions)
    ]
instance RandomFun (Bool, Double) Bool where
  randomFun = choice
    [ (0.5, return Fst)
    , (0.5, compositions)
    ]
instance RandomFun (Bool, Bool) Bool where
  randomFun = choice
    [ (0.5, return And)
    , (0.2, return Fst)
    , (0.2, return Snd)
    , (0.1, compositions)
    ]
instance RandomFun (Bool, Bool) Double where
  randomFun = choice
    [ (0.5, Const <$> normal 0 1)
    , (0.5, compositions)
    ]

instance RandomFun (Double, Double) Bool where
  randomFun = choice
    [(0.5, return Eq)
    , (0.5, compositions)]

instance RandomFun a Ordering where
  randomFun = randomConstFun

instance RandomFun Ordering Bool where
  randomFun = compositions

instance RandomFun Ordering Double where
  randomFun = compositions

instance (RandomFun a b, RandomFun a c) => RandomFun a (b, c) where
  randomFun = pars

class RandomFun a b where
  randomFun :: MonadDistribution m => m (Fun a b)

class RandomConst a where
  randomConst :: MonadDistribution m => m a

randomConstFun :: (MonadDistribution m, RandomConst a) => m (Fun arb a)
randomConstFun = Const <$> randomConst

instance RandomConst Double where
  randomConst = normal 0 1 -- FIXME Add other dists?

instance RandomConst Bool where
  randomConst = choice
    [(0.5, return True)
    , (0.5, return False)
    ]

instance RandomConst Ordering where
  randomConst = choice
    [(0.3, return GT)
    , (0.3, return LT)
    , (0.4, return EQ)
    ]

instance (RandomConst a, RandomConst b) => RandomConst (a, b) where
  randomConst = (,) <$> randomConst <*> randomConst

data Dist a b where
  Normal :: Dist (Double, Double) Double

interpretDist :: MonadDistribution m => Dist a b -> a -> m b
interpretDist Normal (mu, sigma) = normal mu sigma


-- FIXME need some coverage test to make sure all important constructions are eventually produced
