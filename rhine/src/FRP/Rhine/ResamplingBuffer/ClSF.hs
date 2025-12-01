{- |
Collect and process all incoming values statefully and with time stamps.
-}
module FRP.Rhine.ResamplingBuffer.ClSF where

-- transformers
import Control.Monad.Trans.Reader (ReaderT, runReaderT)

-- automaton
import Data.Automaton hiding (toStreamT)
import Data.Stream
import Data.Stream.Optimized (toStreamT)
import Data.Stream.Result (mapResultState)

-- rhine
import FRP.Rhine.ClSF.Core hiding (toStreamT)
import FRP.Rhine.ResamplingBuffer

{- | Given a clocked signal function that accepts
   a varying number of timestamped inputs (a list),
   a `ResamplingBuffer` can be formed
   that collects all this input and steps the signal function
   whenever output is requested.
-}
clsfBuffer ::
  (Monad m) =>
  -- | The clocked signal function that consumes
  --   and a list of timestamped inputs,
  --   and outputs a single value.
  --   The list will contain the /newest/ element in the head.
  ClSF m cl2 [(TimeInfo cl1, a)] b ->
  ResamplingBuffer m cl1 cl2 a b
clsfBuffer = clsfBuffer' . toStreamT . getAutomaton
  where
    clsfBuffer' ::
      (Monad m) =>
      StreamT (ReaderT [(TimeInfo cl1, a)] (ReaderT (TimeInfo cl2) m)) b ->
      ResamplingBuffer m cl1 cl2 a b
    clsfBuffer' StreamT {state, step} =
      ResamplingBuffer
        { buffer = (state, [])
        , put = \ti1 a (s, as) -> pure (s, (ti1, a) : as)
        , get = \ti2 (s, as) -> mapResultState (,[]) <$> runReaderT (runReaderT (step s) as) ti2
        }
