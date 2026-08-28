module Main (main) where

import MarkovianDifferential qualified

main :: IO ()
main = do
    MarkovianDifferential.tests
    putStrLn "PASS: markovian-neural integration"
