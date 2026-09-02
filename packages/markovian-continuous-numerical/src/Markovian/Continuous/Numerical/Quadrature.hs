{-# LANGUAGE BangPatterns #-}

{- | Deterministic bounded adaptive Gauss--Kronrod 15/7 quadrature.
The embedded difference is an error estimate, never a certified bound.
-}
module Markovian.Continuous.Numerical.Quadrature (
    IntegrationTolerance,
    integrationTolerance,
    QuadratureLimits (..),
    QuadratureReport,
    quadratureValue,
    estimatedAbsoluteError,
    evaluationsPerformed,
    subdivisionsPerformed,
    maximumDepthReached,
    QuadratureTermination (..),
    quadratureTermination,
    QuadratureError (..),
    integrateGK15,
) where

import Control.Monad (when)
import Data.List (foldl')
import Markovian.Continuous.Numerical.Value
import Numeric.Natural (Natural)

-- | Named nonnegative absolute and relative tolerances.
data IntegrationTolerance = IntegrationTolerance Double Double
    deriving stock (Eq, Show)

-- | Evaluation, subdivision, and depth limits.
data QuadratureLimits = QuadratureLimits
    { maximumEvaluations :: Natural
    , maximumSubdivisions :: Natural
    , maximumDepth :: Natural
    }
    deriving stock (Eq, Show)

-- | The only successful termination reason.
data QuadratureTermination = EstimatedToleranceMet
    deriving stock (Eq, Show)

-- | A deterministic estimate, estimated error, and work report.
data QuadratureReport = QuadratureReport Double Double Natural Natural Natural QuadratureTermination
    deriving stock (Eq, Show)

-- | A validation, callback, nonfinite, stall, or budget failure.
data QuadratureError
    = InvalidTolerance Double Double
    | InvalidQuadratureLimits
    | EvaluationBudgetExhausted Natural
    | SubdivisionBudgetExhausted Natural
    | DepthBudgetExhausted Natural
    | IntegrandFailure Natural Double String
    | NonFiniteIntegrand Natural Double Double
    | NonFiniteAggregate String Double
    | SubdivisionStalled Double Double
    deriving stock (Eq, Show)

data Piece = Piece !Double !Double !Double !Double !Natural !Natural

-- | Validate the absolute and relative tolerances.
integrationTolerance :: Double -> Double -> Either QuadratureError IntegrationTolerance
integrationTolerance absolute relative
    | any (\value -> isNaN value || isInfinite value || value < 0) [absolute, relative] = Left (InvalidTolerance absolute relative)
    | absolute == 0 && relative == 0 = Left (InvalidTolerance absolute relative)
    | otherwise = Right (IntegrationTolerance absolute relative)

-- | Integrate on one finite interval with bounded adaptive GK15/7.
integrateGK15 :: QuadratureLimits -> IntegrationTolerance -> CompactDoubleInterval -> (Double -> Either String Double) -> Either QuadratureError QuadratureReport
integrateGK15 limits tolerance interval callback
    | maximumEvaluations limits < 15 || maximumDepth limits == 0 = Left InvalidQuadratureLimits
    | otherwise = do
        let (lower, upper) = doubleIntervalBounds interval
        initial <- evaluatePiece callback 0 0 0 lower upper
        loop callback tolerance limits 15 0 0 [initial]

loop :: (Double -> Either String Double) -> IntegrationTolerance -> QuadratureLimits -> Natural -> Natural -> Natural -> [Piece] -> Either QuadratureError QuadratureReport
loop callback tolerance limits !evaluations !subdivisions !nextId pieces = do
    let !totalValue = compensated [value | Piece _ _ value _ _ _ <- pieces]
        !totalError = compensated [err | Piece _ _ _ err _ _ <- pieces]
        !depthReached = foldr max 0 [depth | Piece _ _ _ _ depth _ <- pieces]
    ensureFiniteAggregate "integral estimate" totalValue
    ensureFiniteAggregate "aggregate estimated error" totalError
    toleranceMet <- meets tolerance totalValue totalError
    if toleranceMet
        then Right (QuadratureReport totalValue totalError evaluations subdivisions depthReached EstimatedToleranceMet)
        else do
            when (subdivisions >= maximumSubdivisions limits) (Left (SubdivisionBudgetExhausted subdivisions))
            when (evaluations + 30 > maximumEvaluations limits) (Left (EvaluationBudgetExhausted evaluations))
            let selected@(Piece lower upper _ _ depth _) = selectPiece pieces
            when (depth >= maximumDepth limits) (Left (DepthBudgetExhausted depth))
            let midpoint = lower + (upper - lower) / 2
            when (midpoint == lower || midpoint == upper) (Left (SubdivisionStalled lower upper))
            left <- evaluatePiece callback evaluations (nextId + 1) (depth + 1) lower midpoint
            right <- evaluatePiece callback (evaluations + 15) (nextId + 2) (depth + 1) midpoint upper
            let remaining = removePiece selected pieces
            loop callback tolerance limits (evaluations + 30) (subdivisions + 1) (nextId + 2) (left : right : remaining)

meets :: IntegrationTolerance -> Double -> Double -> Either QuadratureError Bool
meets (IntegrationTolerance absolute relative) value err = do
    let threshold = max absolute (relative * abs value)
    ensureFiniteAggregate "tolerance threshold" threshold
    pure (err <= threshold)

ensureFiniteAggregate :: String -> Double -> Either QuadratureError ()
ensureFiniteAggregate phase value
    | isNaN value || isInfinite value = Left (NonFiniteAggregate phase value)
    | otherwise = Right ()

selectPiece :: [Piece] -> Piece
selectPiece (first : rest) = foldl' prefer first rest
  where
    prefer best@(Piece bestLower _ _ bestError _ bestId) candidate@(Piece lower _ _ err _ identifier)
        | err > bestError = candidate
        | err < bestError = best
        | lower < bestLower = candidate
        | lower > bestLower = best
        | identifier < bestId = candidate
        | otherwise = best
selectPiece [] = error "integrateGK15 internal invariant: no active pieces"

removePiece :: Piece -> [Piece] -> [Piece]
removePiece selected = removeFirst
  where
    removeFirst [] = []
    removeFirst (piece : rest)
        | pieceId piece == pieceId selected = rest
        | otherwise = piece : removeFirst rest
    pieceId (Piece _ _ _ _ _ identifier) = identifier

evaluatePiece :: (Double -> Either String Double) -> Natural -> Natural -> Natural -> Double -> Double -> Either QuadratureError Piece
evaluatePiece callback startingIndex identifier depth lower upper = do
    let center = lower + (upper - lower) / 2
        half = (upper - lower) / 2
        points = center : concat [[center - half * node, center + half * node] | node <- drop 1 kronrodNodes]
    values <- sequence [evaluate callback (startingIndex + fromIntegral index) point | (index, point) <- zip [(1 :: Int) ..] points]
    case values of
        centerValue : remainingValues ->
            case pairUp remainingValues of
                [pair1, pair2, pair3, pair4, pair5, pair6, pair7] -> do
                    let pairs = [pair1, pair2, pair3, pair4, pair5, pair6, pair7]
                        pairTotal (leftValue, rightValue) = leftValue + rightValue
                        kronrod = half * (0.20948214108472782 * centerValue + sum [weight * pairTotal pair | (weight, pair) <- zip (drop 1 kronrodWeights) pairs])
                        gauss = half * (0.4179591836734694 * centerValue + 0.3818300505051189 * pairTotal pair2 + 0.27970539148927667 * pairTotal pair4 + 0.1294849661688697 * pairTotal pair6)
                        err = abs (kronrod - gauss)
                    if any (\value -> isNaN value || isInfinite value) [kronrod, gauss, err]
                        then Left (NonFiniteIntegrand startingIndex center kronrod)
                        else Right (Piece lower upper kronrod err depth identifier)
                _ -> Left (SubdivisionStalled lower upper)
        [] -> Left (SubdivisionStalled lower upper)

-- Standard QUADPACK QK15 positive abscissae, ordered from center outward.
kronrodNodes :: [Double]
kronrodNodes = [0, 0.20778495500789847, 0.40584515137739717, 0.5860872354676911, 0.7415311855993945, 0.8648644233597691, 0.9491079123427585, 0.9914553711208126]

kronrodWeights :: [Double]
kronrodWeights = [0.20948214108472782, 0.20443294007529889, 0.19035057806478542, 0.1690047266392679, 0.14065325971552592, 0.10479001032225018, 0.06309209262997855, 0.022935322010529225]

evaluate :: (Double -> Either String Double) -> Natural -> Double -> Either QuadratureError Double
evaluate callback index point = case callback point of
    Left err -> Left (IntegrandFailure index point err)
    Right value
        | isNaN value || isInfinite value -> Left (NonFiniteIntegrand index point value)
        | otherwise -> Right value

pairUp :: [value] -> [(value, value)]
pairUp (left : right : rest) = (left, right) : pairUp rest
pairUp [] = []
pairUp _ = error "integrateGK15 internal invariant: odd node values"

compensated :: [Double] -> Double
compensated = fst . foldl' step (0, 0)
  where
    step (!total, !correction) value =
        let adjusted = value - correction
            next = total + adjusted
         in (next, (next - total) - adjusted)

-- | Get the integral estimate.
quadratureValue :: QuadratureReport -> Double
quadratureValue (QuadratureReport value _ _ _ _ _) = value

-- | Get the embedded absolute error estimate.
estimatedAbsoluteError :: QuadratureReport -> Double
estimatedAbsoluteError (QuadratureReport _ err _ _ _ _) = err

-- | Get the callback evaluation count.
evaluationsPerformed :: QuadratureReport -> Natural
evaluationsPerformed (QuadratureReport _ _ count _ _ _) = count

-- | Get the completed subdivision count.
subdivisionsPerformed :: QuadratureReport -> Natural
subdivisionsPerformed (QuadratureReport _ _ _ count _ _) = count

-- | Get the largest completed subdivision depth.
maximumDepthReached :: QuadratureReport -> Natural
maximumDepthReached (QuadratureReport _ _ _ _ depth _) = depth

-- | Get the successful termination reason.
quadratureTermination :: QuadratureReport -> QuadratureTermination
quadratureTermination (QuadratureReport _ _ _ _ _ termination) = termination
