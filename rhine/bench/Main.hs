-- criterion
import Criterion.Main

-- dunai
import Data.MonadicStreamFunction qualified as Dunai

-- rhine
import WordCount

main :: IO ()
main = defaultMain [WordCount.benchmarks]
