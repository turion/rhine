-- criterion
import Criterion.Main

-- rhine
import Sum
import WordCount

main :: IO ()
main = defaultMain [WordCount.benchmarks, Sum.benchmarks]
