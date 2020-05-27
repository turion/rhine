-- FIXME rename to Exports?
module FRP.Rhine.Bayes.Internal
  ( module X ) where


-- dunai
import Control.Monad.Trans.MSF.Reader as X (readerS, runReaderS)

-- dunai-bayes
import Data.MonadicStreamFunction.Bayes as X (runPopulationS)
