module Main (main) where

import ClarkScarf1960 (runClarkScarf1960Tests)
import DogruInventoryBenchmark (runDogruInventoryTests)
import FixedBatchRnQ (runFixedBatchRnQTests)
import InventoryBenchmark (runInventoryBenchmarkTests)

main :: IO ()
main = do
    runInventoryBenchmarkTests run
    runClarkScarf1960Tests run
    runDogruInventoryTests run
    runFixedBatchRnQTests run
  where
    run name action = action >> putStrLn ("PASS: " ++ name)
