{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module GeneralLearning where
data MyType = NumberType | BooleanType | Function MyType MyType

data Value (t :: MyType) where
  Number :: Num a => a -> Value 'NumberType
  Boolean :: Bool -> Value 'BooleanType

instance Num (Value :: 'NumberType) where
  Number a1 + Number a2 = Number $ a1 + a

