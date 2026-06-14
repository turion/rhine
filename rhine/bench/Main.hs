-- criterion
import Criterion.Main

-- rhine
import Sum
import WordCount
import SumMultirate

main :: IO ()
main = defaultMain [WordCount.benchmarks, Sum.benchmarks, SumMultirate.benchmarks]
