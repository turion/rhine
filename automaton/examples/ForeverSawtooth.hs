-- base
import Control.Arrow ((>>>))
import Control.Monad (guard)

-- automaton
import Data.Automaton
import Data.Automaton.Trans.Except

sawtooth :: Automaton IO a Int
sawtooth = forever $ try $ count >>> throwOnMaybe (\n -> guard (n > 10))

main :: IO ()
main = reactimate $ sawtooth >>> arrM print
