-- base
import Control.Arrow ((>>>))
import Control.Monad (guard)

-- automaton
import Data.Automaton
import Data.Automaton.Trans.Except

userSawtooth :: Int -> Automaton IO a Int
userSawtooth nMax = safely $ do
  try $ count >>> throwOnMaybe (\n -> guard (n > nMax))
  nMax' <- once_ $ do
    putStrLn "Maximum reached, please enter next nMax:"
    readLn
  safe $ userSawtooth nMax'

main :: IO ()
main = reactimate $ userSawtooth 10 >>> arrM print
