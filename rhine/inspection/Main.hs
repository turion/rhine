{-# LANGUAGE TemplateHaskell #-}

-- base
import Data.Functor.Identity

-- inspection-testing
import Test.Inspection

-- automaton
import Data.Stream
import Data.Automaton

-- rhine
import FRP.Rhine

-- two :: (Applicative m) => StreamT m Int
-- two = pure 2

twoApplicative :: (Applicative m) => StreamT m Int
twoApplicative = (*) <$> pure 1 <*> pure 2

id1 :: ClSF Identity cl Int Int
id1 = clId

idid :: ClSF Identity cl Int Int
idid = clId >>> clId

-- inspect $ 'two === 'twoApplicative
-- inspect $ 'idid === 'id1
inspect $ 'idid `hasNoType` 'JointState
-- inspect $ 'two `hasNoType` 'JointState
-- inspect $ 'twoApplicative `hasNoType` 'JointState -- Report upstream bug, right now this passes

main = return ()
