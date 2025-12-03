-- | 'AccumT' example
module Main where

-- base
import Control.Monad (void)
import Control.Monad.IO.Class

-- transformers
import Control.Monad.Trans.Accum (AccumT, add, look, runAccumT)

-- text
import Data.Text (Text, pack)

-- rhine
import FRP.Rhine hiding (add)

personalMessage :: ClSF (AccumT [Text] IO) (Lifting StdinClock) () ()
personalMessage = tagS >>> arrMCl (pure >>> add)

type Lifting = LiftClock IO (AccumT [Text])
type EveryTwoSeconds = Lifting (Millisecond 2000)

everyTwoSeconds :: EveryTwoSeconds
everyTwoSeconds = liftClock waitClock

logSoFar :: ClSF (AccumT [Text] IO) EveryTwoSeconds () ()
logSoFar = sinceStart >>> arrMCl printLog
  where
    printLog t = do
      add ["Time since start: " <> pack (show t)]
      l <- look
      liftIO $ do
        putStrLn "Log so far:"
        print l

main :: IO ()
main = do
  putStrLn "You can add a personal message to the log by typing and pressing enter."
  void $ flip runAccumT [] $ flow $ personalMessage @@ liftClock StdinClock |@| logSoFar @@ everyTwoSeconds
