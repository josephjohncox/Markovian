module Main (main) where

import Control.Monad (forM_, unless)
import Markovian.Continuous.Internal

limits :: ExactLimits
limits = ExactLimits 16 1000 100000 10000 100 1000000 2048

-- Compiled against the private source by check-continuous-boundary, not
-- installed or exposed. Malformed values cannot be built by public clients.
main :: IO ()
main = do
    let interval = RationalInterval 0 1
        term = (1, 1, interval)
        dirac = ExactLaw 0 []
        malformed = ExactJointLaw (ExactLaw 0 (repeat term)) dirac
        run = pairedDifferenceInternal
    left (TermLimitExceeded 5 6) (run limits{limitTerms = 5, limitDegree = 0} malformed)
    left (TermLimitExceeded 5 6) (run limits{limitTerms = 5, limitDegree = 0} (ExactJointLaw dirac (ExactLaw 0 (repeat term))))
    left DuplicateNoiseOwner (run limits (ExactJointLaw (ExactLaw 0 [term, term]) dirac))
    left DuplicateNoiseOwner (run limits (ExactJointLaw (ExactLaw 0 [(2, 1, interval), term]) dirac))
    let conflict = ExactJointLaw (ExactLaw 0 [term]) (ExactLaw 1024 [(1, 1, RationalInterval 0 2)])
    left (WorkLimitExceeded 10 11) (run limits{limitWork = 10, limitRationalBits = 1} conflict)
    left InconsistentSharedNoise (run limits{limitRationalBits = 1} conflict)
    let disjoint = ExactJointLaw (ExactLaw 1024 [term]) (ExactLaw 0 [(2, 1, interval)])
    left (NoiseOwnerLimitExceeded 1 2) (run limits{limitNoiseOwners = 1, limitRationalBits = 1} disjoint)
    left (InvalidInterval 1 1) (run limits (ExactJointLaw (ExactLaw 0 [(1, 1, RationalInterval 1 1)]) dirac))
    forM_ [1, 2, 5, 10, 100, fromIntegral (maxBound :: Int)] $ \cap ->
        forM_ [(0, 0), (1, 1), (3, 7), (cap, cap), (cap - 1, cap - 1)] $ \(n, m) ->
            assert "saturating preflight independent Integer oracle" (pairedPreflightWork cap n m == fromInteger (min (toInteger cap) (let s = toInteger n + toInteger m in s + s * s + 5)))
    putStrLn "PASS: paired private invariant, bounded-spine and saturation probes"

left :: (Eq e, Show e) => e -> Either e a -> IO ()
left expected result = case result of
    Left actual -> assert ("expected " ++ show expected ++ ", got " ++ show actual) (actual == expected)
    Right _ -> fail ("expected atomic failure " ++ show expected)

assert :: String -> Bool -> IO ()
assert label condition = unless condition (fail label)
