-- criterion
import Criterion.Main

-- rhine
import Sum
import SumMultirate
import WordCount

main :: IO ()
main = defaultMain [WordCount.benchmarks, Sum.benchmarks, SumMultirate.benchmarks]
