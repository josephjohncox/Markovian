module Main (main) where

import TabularLearning (runTabularLearningTests)

main :: IO ()
main = runTabularLearningTests run
  where
    run name action = action >> putStrLn ("PASS: " ++ name)
