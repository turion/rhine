module Mod where

import Data.Automaton (AutomatonT)
import FRP.Rhine

idid :: Monad m => ClSF m cl a a
idid = clId >>> clId

twoApplicative :: (Applicative m) => AutomatonT m Int
twoApplicative = (*) <$> pure 1 <*> pure 2
