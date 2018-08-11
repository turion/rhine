{-# LANGUAGE TypeFamilies #-}

import FRP.Rhine
import FRP.Rhine.Reactimation.Combinators

rhine1 :: Rhine m cl1 a b
rhine1 = undefined

rhine2 :: Rhine m cl1 a b
rhine2 = undefined

schedule :: Schedule m cl1 cl2
schedule = undefined

parRhine
  :: (Monad m --, Time cl1 ~ Time (Out cl1), Time cl2 ~ Time (Out cl2), Time cl1 ~ Time (In cl1), Time cl2 ~ Time (In cl2)
     , Clock m cl1, Clock m cl2)
  => Rhine m (ParClock m cl1 cl2) a b
parRhine = rhine1 ||@ schedule @|| rhine2

main = return ()
