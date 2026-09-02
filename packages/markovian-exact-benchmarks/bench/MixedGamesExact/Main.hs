{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.Bits (xor)
import Data.Char (ord)
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Version (showVersion)
import Data.Word (Word64)
import Markovian.Category.Finite.Object
import Markovian.Game.NormalForm.Exact
import Markovian.Game.Profile.Finite
import System.CPUTime (getCPUTime)
import System.Info (arch, compilerName, compilerVersion, os)
import Text.Printf (printf)

data Player = Row | Column deriving (Eq, Show)
data Action = Heads | Tails deriving (Eq, Show)

data Fixture = Fixture (ExactNormalGame Player Action) (ExactMixedProfile Player Action)

limits :: GameLimits
limits = gameLimits 4 4 100 1000 100000 4096 4

main :: IO ()
main = do
    fixture <- buildFixture
    _ <- runOnce fixture
    samples <- replicateM 20 (runOnce fixture)
    let reports = map snd samples
        times = map fst samples
    if all (== head reports) (tail reports) then pure () else fail "mixed-game semantic report changed"
    let checksum = semanticChecksum (head reports)
    if checksum == 0x4acdf5eb089505cc then pure () else fail ("mixed-game semantic checksum changed: " ++ show checksum)
    putStrLn "markovian exact mixed-game candidate-check benchmark"
    putStrLn ("host-os: " ++ os)
    putStrLn ("host-arch: " ++ arch)
    putStrLn ("compiler: " ++ compilerName ++ "-" ++ showVersion compilerVersion)
    putStrLn "clock: process CPU time"
    putStrLn "warmups: 1 (excluded)"
    putStrLn "samples: 20"
    putStrLn ("raw-seconds: " ++ intercalate "," (map (printf "%.9f") times))
    printf "mean-seconds: %.9f\n" (mean times)
    printf "sample-deviation-seconds: %.9f\n" (sampleDeviation times)
    printf "semantic-checksum-fnv1a64: 0x%016x\n" checksum
    putStrLn ("semantic-report: " ++ head reports)

runOnce :: Fixture -> IO (Double, String)
runOnce (Fixture game mixed) = do
    start <- getCPUTime
    report <- either (fail . show) pure (checkMixedNash limits game mixed)
    let rendered = show report
    _ <- evaluate (length rendered)
    end <- getCPUTime
    pure (fromIntegral (end - start) / 1.0e12, rendered)

buildFixture :: IO Fixture
buildFixture = do
    owners <- checked (finiteObject [Row, Column])
    actions <- checked (finiteObject [Heads, Tails])
    product_ <- checked (ownedProduct limits owners [(Row, actions), (Column, actions)])
    let profiles = NonEmpty.toList (finiteObjectValues (ownedProfiles product_))
    payoffs <- traverse (payoff owners) profiles
    game <- checked (exactNormalGame limits product_ payoffs)
    half <- checked (exactSimplex limits actions [(Heads, 1 / 2), (Tails, 1 / 2)])
    mixed <- checked (exactMixedProfile limits product_ [(Row, half), (Column, half)])
    pure (Fixture game mixed)
  where
    payoff owners profile = do
        let same = profileChoice profile Row == profileChoice profile Column
        values <- checked (exactPlayerValues limits owners [(Row, if same then 1 else -1), (Column, if same then -1 else 1)])
        pure (profile, values)

checked :: (Show error) => Either error value -> IO value
checked = either (fail . show) pure

mean :: [Double] -> Double
mean values = sum values / fromIntegral (length values)

sampleDeviation :: [Double] -> Double
sampleDeviation values = sqrt (sum [(value - average) ^ (2 :: Int) | value <- values] / fromIntegral (length values - 1))
  where
    average = mean values

semanticChecksum :: String -> Word64
semanticChecksum = foldl step 14695981039346656037
  where
    step hash character = (hash `xor` fromIntegral (ord character)) * 1099511628211
