-- criterion
import Criterion.Main

-- rhine
import WordCount

main :: IO ()
main = defaultMain [WordCount.benchmarks]
