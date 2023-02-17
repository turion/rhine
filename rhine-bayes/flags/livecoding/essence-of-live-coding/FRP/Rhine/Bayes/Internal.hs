module FRP.Rhine.Bayes.Internal (
  module X,
  readerS,
  runReaderS,
  runPopulationS,
) where

-- transformers
import Control.Monad.Trans.Reader (ReaderT (..))

-- log-domain
import Numeric.Log (Log)

-- monad-bayes
import Control.Monad.Bayes.Population (Population)

-- dunai-bayes
-- FIXME move to rhine-bayes or get rid of it completely? Or move to separate package? Or to rhine-bayes?
import Data.MonadicStreamFunction.Bayes as X (SoftEq)

-- essence-of-live-coding
import LiveCoding (Cell, readerC', runReaderC')

-- essence-of-live-coding-bayes
import LiveCoding.Bayes (runPopulationC)

runReaderS ::
  Monad m =>
  Cell (ReaderT r m) a b ->
  Cell m (r, a) b
runReaderS = runReaderC'

readerS ::
  Monad m =>
  Cell m (r, a) b ->
  Cell (ReaderT r m) a b
readerS = readerC'

runPopulationS ::
  forall m a b.
  Monad m =>
  Int ->
  (forall x. Population m x -> Population m x) ->
  Cell (Population m) a b ->
  Cell m a [(b, Log Double)]
runPopulationS = runPopulationC
