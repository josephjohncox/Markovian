{- | Exact rational finite decisions and deterministic equilibrium reports.

The decision constructor enumerates represented pure functions before building
tables.  Rational comparison is literal and every maximizer is retained.
-}
module Markovian.Game.Open.Exact (
    ExactDecisionError (..),
    exactMaximizingDecision,
    ExactContextError (..),
    contextFromExactPayoff,
    renderEquilibriumReport,
) where

import Data.List (intercalate)
import Markovian.Category.Finite.Set (FiniteSet, finiteSet, finiteSetValues, sameFiniteSet)
import Markovian.Category.Payoff.Exact
import Markovian.Game.Open.Finite
import Markovian.Game.Optic.Finite
import Numeric.Natural (Natural)

-- | Exact decision construction failures.
data ExactDecisionError
    = ExactDecisionStrategyLimitExceeded !Natural !Natural
    | ExactDecisionWorkLimitExceeded !Natural !Natural
    | ExactDecisionInternalConstructionFailure
    deriving (Eq, Show)

{- | Construct one owner-controlled exact decision, specialized from the
selection-function decision open game in Ghani, Hedges, Winschel, and Zahn,
/Compositional Game Theory/ (LICS 2018), Definition 4, DOI
@10.1145/3209108.3209165@.

Strategies are all represented functions @X -> Y@.  At the current input a
deviation is a best response exactly when its continuation value is maximal.
Off-path values of the strategy remain represented and are not quotient out.
-}
exactMaximizingDecision ::
    (Eq owner, Eq x, Eq y) =>
    FiniteBudget ->
    owner ->
    FiniteSet x ->
    FiniteSet y ->
    FiniteSet Rational ->
    Either ExactDecisionError (FiniteOpenGame owner (FiniteFunction x y) x () y Rational)
exactMaximizingDecision budget owner inputs outputs utilities = do
    let strategyCount = functionSpaceCardinality inputs outputs
        enumerationWork = strategyCount + strategyCount * naturalLength (finiteSetValues inputs)
    if enumerationWork > maximumFiniteWork budget
        then Left (ExactDecisionStrategyLimitExceeded enumerationWork (maximumFiniteWork budget))
        else do
            strategies <- mapLeft (const ExactDecisionInternalConstructionFailure) (enumerateFiniteFunctions budget inputs outputs)
            strategySet <- case finiteSet strategies of
                Right result -> Right result
                Left _ -> Left ExactDecisionInternalConstructionFailure
            unit <- case finiteSet [()] of
                Right result -> Right result
                Left _ -> Left ExactDecisionInternalConstructionFailure
            let schema = ownedStrategySchema owner strategySet
                playEntries =
                    [ ((strategy, input), applyRequired strategy input)
                    | strategy <- strategies
                    , input <- finiteSetValues inputs
                    ]
                coplayEntries =
                    [ ((strategy, input, utility), ())
                    | strategy <- strategies
                    , input <- finiteSetValues inputs
                    , utility <- finiteSetValues utilities
                    ]
                response input continuation _incumbent deviation =
                    let deviationValue = applyRequired continuation (applyRequired deviation input)
                        representedValues =
                            [ applyRequired continuation (applyRequired candidate input)
                            | candidate <- strategies
                            ]
                     in all (deviationValue >=) representedValues
            case finiteOpenGame budget schema inputs unit outputs utilities playEntries coplayEntries response of
                Right game -> Right game
                Left (OpenGameWorkLimitExceeded requiredWork limit) -> Left (ExactDecisionWorkLimitExceeded requiredWork limit)
                Left _ -> Left ExactDecisionInternalConstructionFailure

-- | Exact payoff-to-context failures.
data ExactContextError x y
    = ExactContextPayoffTargetMismatch
    | ExactContextUtilityOutsideCarrier !y !Rational
    | ExactContextFunctionError !(FiniteFunctionError y Rational)
    | ExactContextBoundaryError !(OpenGameContextError x)
    deriving (Eq, Show)

{- | Build a checked context from an exact payoff whose values belong to the
game's explicitly represented rational utility carrier.
-}
contextFromExactPayoff ::
    (Eq strategy, Eq x, Eq y) =>
    FiniteBudget ->
    FiniteOpenGame owner strategy x s y Rational ->
    x ->
    ExactPayoff y ->
    Either (ExactContextError x y) (OpenGameContext x y Rational)
contextFromExactPayoff budget game input payoff
    | not (sameFiniteSet (exactPayoffObject payoff) (openGameTarget game)) = Left ExactContextPayoffTargetMismatch
    | Just (output, utility) <- firstOutsideUtility = Left (ExactContextUtilityOutsideCarrier output utility)
    | otherwise = do
        continuation <-
            mapLeft ExactContextFunctionError $
                finiteFunction budget (openGameTarget game) (openGameUtility game) (exactPayoffValues payoff)
        mapLeft ExactContextBoundaryError (openGameContext game input continuation)
  where
    firstOutsideUtility =
        firstMatch
            (\(_, utility) -> utility `notElem` finiteSetValues (openGameUtility game))
            (exactPayoffValues payoff)

{- | Render deterministic semantic evidence, including exact incumbent utility.
There are deliberately no timing fields.
-}
renderEquilibriumReport ::
    (Show strategy, Show r, Eq strategy, Eq x, Eq y) =>
    FiniteOpenGame owner strategy x s y r ->
    OpenGameContext x y r ->
    EquilibriumReport strategy ->
    String
renderEquilibriumReport game context report =
    unlines
        [ "Finite pure contextual equilibrium report"
        , "represented profiles: " ++ show (representedProfileCount report)
        , "relation checks: " ++ show (performedRelationChecks report)
        , "profile limit: " ++ show (configuredProfileLimit report)
        , "relation limit: " ++ show (configuredRelationLimit report)
        , "completed: " ++ show (equilibriumCompleted report)
        , "equilibria: [" ++ intercalate ", " (map renderProfile (equilibriumProfiles report)) ++ "]"
        ]
  where
    renderProfile strategy =
        let output = required "report play" (playOpenGame game strategy (contextInput context))
            utility = applyRequired (contextContinuation context) output
         in show strategy ++ " => " ++ show utility

naturalLength :: [value] -> Natural
naturalLength = fromIntegral . length

applyRequired :: (Eq input) => FiniteFunction input output -> input -> output
applyRequired function input = required "exact finite function" (applyFiniteFunction function input)

required :: String -> Maybe value -> value
required _ (Just value) = value
required label Nothing = error ("checked exact open-game lookup failed: " ++ label)

firstMatch :: (value -> Bool) -> [value] -> Maybe value
firstMatch _ [] = Nothing
firstMatch predicate (value : remaining)
    | predicate value = Just value
    | otherwise = firstMatch predicate remaining

mapLeft :: (left -> other) -> Either left value -> Either other value
mapLeft operation (Left problem) = Left (operation problem)
mapLeft _ (Right value) = Right value
