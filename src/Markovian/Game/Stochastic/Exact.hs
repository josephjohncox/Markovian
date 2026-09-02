{-# LANGUAGE RoleAnnotations #-}

{- | Exact finite-horizon public-state stochastic games.

The scope is simultaneous action, perfect monitoring, independent private
randomization, a public state, and a finite horizon.  Each transition law is a
joint law of reward vector and successor.  This preserves pathwise correlation;
the current additive expectation itself is determined by the marginals.
Terminal status is checked before
horizon and terminal utility is paid once without requesting an action.
-}
module Markovian.Game.Stochastic.Exact (
    ExactStochasticGame,
    StochasticGameError (..),
    exactStochasticGame,
    stochasticPlayers,
    stochasticStates,
    stochasticActions,
    stochasticDiscount,
    stochasticTerminalValue,
    stochasticTransition,
    ExactFiniteHorizonMarkovProfile,
    MarkovProfileError (..),
    exactFiniteHorizonMarkovProfile,
    markovProfileHorizon,
    markovStrategyAt,
    StochasticEvaluationError (..),
    StochasticValueReport (..),
    evaluateMarkovProfile,
    stochasticValueAt,
    MarkovPerfectCheck (..),
    MarkovPerfectReport (..),
    checkFiniteHorizonMarkovPerfect,
) where

import Control.Monad (join)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, isNothing)
import Markovian.Category.Finite.Object
import Markovian.Game.NormalForm.Exact
import Markovian.Game.Outcome.Exact
import Markovian.Game.Profile.Finite
import Markovian.Horizon
import Numeric.Natural (Natural)

{-# ANN module ("HLint: ignore Use when" :: String) #-}

-- | Checked exact dynamic game.
data ExactStochasticGame owner state action
    = ExactStochasticGame
        !(FiniteObject state)
        !(OwnedProduct owner action)
        !Rational
        ![(state, Maybe (ExactPlayerValues owner))]
        ![((state, OwnedProfile owner action), ExactOutcomeLaw (ExactStageOutcome owner state))]
    deriving (Eq, Show)

type role ExactStochasticGame nominal nominal nominal

-- | Dynamic-game construction failures.
data StochasticGameError owner state action
    = InvalidStochasticDiscount !Rational
    | StochasticDiscountRationalLimitExceeded !Natural !Natural
    | ExcessTerminalRows
    | DuplicateTerminalState !state
    | MissingTerminalState !state
    | TerminalStateOutsideCarrier !state
    | TerminalPlayerLayoutMismatch !state
    | ExcessTransitionRows
    | DuplicateTransitionRow !state !(OwnedProfile owner action)
    | MissingTransitionRow !state !(OwnedProfile owner action)
    | TransitionRowForTerminal !state !(OwnedProfile owner action)
    | TransitionStateOutsideCarrier !state
    | TransitionProfileOutsideProduct !state !(OwnedProfile owner action)
    | TransitionRewardPlayerLayoutMismatch !state !(OwnedProfile owner action)
    | TransitionSuccessorOutsideCarrier !state !(OwnedProfile owner action) !state
    | StochasticGameCellLimitExceeded !Natural !Natural
    | StochasticGameWorkLimitExceeded !Natural !Natural
    | StochasticActionProductInvalid
    | TerminalValueRationalLimitExceeded !state !owner !Natural !Natural
    | TransitionLawInvalid !state !(OwnedProfile owner action)
    deriving (Eq, Show)

{- | Validate terminal rows and one complete joint outcome law for each
nonterminal state/pure-action profile.
-}
exactStochasticGame :: (Eq owner, Eq state, Eq action) => GameLimits -> FiniteObject state -> OwnedProduct owner action -> Rational -> [(state, Maybe (ExactPlayerValues owner))] -> [((state, OwnedProfile owner action), ExactOutcomeLaw (ExactStageOutcome owner state))] -> Either (StochasticGameError owner state action) (ExactStochasticGame owner state action)
exactStochasticGame limits states product_ discount suppliedTerminal suppliedTransitions = do
    case validateOwnedProduct limits product_ of
        Left _ -> Left StochasticActionProductInvalid
        Right () -> pure ()
    if discount < 0 || discount > 1 then Left (InvalidStochasticDiscount discount) else pure ()
    case checkRationalSize limits discount of
        Left (actual, maximum_) -> Left (StochasticDiscountRationalLimitExceeded actual maximum_)
        Right _ -> pure ()
    let stateValues = NonEmpty.toList (finiteObjectValues states)
        terminalBounded = take (length stateValues + 1) suppliedTerminal
    if length terminalBounded > length stateValues then Left ExcessTerminalRows else pure ()
    case firstDuplicate (map fst terminalBounded) of
        Just duplicate -> Left (DuplicateTerminalState duplicate)
        Nothing -> pure ()
    case firstOutside stateValues (map fst terminalBounded) of
        Just outside -> Left (TerminalStateOutsideCarrier outside)
        Nothing -> pure ()
    terminalRows <- traverse terminalRow stateValues
    let nonterminal = [state | (state, Nothing) <- terminalRows]
        profileCount = fromIntegral (finiteObjectCardinality (ownedProfiles product_))
        expectedCount = cappedGameProduct (maximumGameCells limits) (fromIntegral (length nonterminal)) profileCount
        cells = cappedGameProduct (maximumGameCells limits) expectedCount (fromIntegral (finiteObjectCardinality (ownedOwners product_)))
    if cells > maximumGameCells limits then Left (StochasticGameCellLimitExceeded cells (maximumGameCells limits)) else pure ()
    let profiles = NonEmpty.toList (finiteObjectValues (ownedProfiles product_))
        expectedKeys = [(state, profile) | state <- nonterminal, profile <- profiles]
        transitionBounded = take (length expectedKeys + 1) suppliedTransitions
        outcomeWork = foldl (cappedGameAdd (maximumGameWork limits)) 0 [fromIntegral (length (outcomeEntries law)) | (_, law) <- transitionBounded]
        structuralWork = cappedGameAdd (maximumGameWork limits) (fromIntegral (length stateValues)) expectedCount
        work = cappedGameAdd (maximumGameWork limits) structuralWork (cappedGameProduct (maximumGameWork limits) outcomeWork 2)
    if work > maximumGameWork limits then Left (StochasticGameWorkLimitExceeded work (maximumGameWork limits)) else pure ()
    if length transitionBounded > length expectedKeys then Left ExcessTransitionRows else pure ()
    case firstDuplicate (map fst transitionBounded) of
        Just (state, profile) -> Left (DuplicateTransitionRow state profile)
        Nothing -> pure ()
    validateSuppliedKeys stateValues profiles terminalRows transitionBounded
    transitions <- traverse (transitionRow transitionBounded) expectedKeys
    traverse_ validateLaw transitions
    Right (ExactStochasticGame states product_ discount terminalRows transitions)
  where
    terminalRow state = case lookup state suppliedTerminal of
        Nothing -> Left (MissingTerminalState state)
        Just terminal
            | maybe False ((/= ownedOwners product_) . playerValuesCarrier) terminal -> Left (TerminalPlayerLayoutMismatch state)
            | otherwise -> do
                case terminal of
                    Nothing -> pure ()
                    Just values -> traverse_ (validateTerminal state) (playerValueEntries values)
                Right (state, terminal)
    transitionRow rows key@(state, profile) = case lookup key rows of
        Nothing -> Left (MissingTransitionRow state profile)
        Just law -> Right (key, law)
    validateTerminal state (owner, value)
        | rationalSizeBits value > maximumGameRationalBits limits = Left (TerminalValueRationalLimitExceeded state owner (rationalSizeBits value) (maximumGameRationalBits limits))
        | otherwise = Right ()
    validateLaw ((state, profile), law) = do
        total <- foldlM (validateMass state profile) 0 (outcomeEntries law)
        if total == 1 then pure () else Left (TransitionLawInvalid state profile)
        traverse_ (validateAtom state profile . fst) (outcomeEntries law)
    validateMass state profile accumulator (_, mass)
        | mass < 0 || rationalSizeBits mass > maximumGameRationalBits limits = Left (TransitionLawInvalid state profile)
        | otherwise =
            let total = accumulator + mass
             in if rationalSizeBits total > maximumGameRationalBits limits then Left (TransitionLawInvalid state profile) else Right total
    validateAtom state profile outcome
        | playerValuesCarrier (stageRewards outcome) /= ownedOwners product_ = Left (TransitionRewardPlayerLayoutMismatch state profile)
        | successorState outcome `notElem` NonEmpty.toList (finiteObjectValues states) = Left (TransitionSuccessorOutsideCarrier state profile (successorState outcome))
        | otherwise = Right ()

validateSuppliedKeys :: (Eq owner, Eq state, Eq action) => [state] -> [OwnedProfile owner action] -> [(state, Maybe values)] -> [((state, OwnedProfile owner action), law)] -> Either (StochasticGameError owner state action) ()
validateSuppliedKeys states profiles terminalRows = traverse_ (validate . fst)
  where
    validate (state, profile)
        | state `notElem` states = Left (TransitionStateOutsideCarrier state)
        | profile `notElem` profiles = Left (TransitionProfileOutsideProduct state profile)
        | maybe False isJust (lookup state terminalRows) = Left (TransitionRowForTerminal state profile)
        | otherwise = Right ()
    isJust (Just _) = True
    isJust Nothing = False

-- | Read players.
stochasticPlayers :: ExactStochasticGame owner state action -> FiniteObject owner
stochasticPlayers (ExactStochasticGame _ product_ _ _ _) = ownedOwners product_

-- | Read public states.
stochasticStates :: ExactStochasticGame owner state action -> FiniteObject state
stochasticStates (ExactStochasticGame states _ _ _ _) = states

-- | Read action ownership/layout.
stochasticActions :: ExactStochasticGame owner state action -> OwnedProduct owner action
stochasticActions (ExactStochasticGame _ product_ _ _ _) = product_

-- | Read the exact one-transition discount.
stochasticDiscount :: ExactStochasticGame owner state action -> Rational
stochasticDiscount (ExactStochasticGame _ _ discount _ _) = discount

{- | Query terminal status and value.  'Nothing' means either nonterminal or an
outside state; membership can be checked through 'stochasticStates'.
-}
stochasticTerminalValue :: (Eq state) => ExactStochasticGame owner state action -> state -> Maybe (ExactPlayerValues owner)
stochasticTerminalValue (ExactStochasticGame _ _ _ rows _) state = join (lookup state rows)

-- | Query one nonterminal joint transition law.
stochasticTransition :: (Eq state, Eq owner, Eq action) => ExactStochasticGame owner state action -> state -> OwnedProfile owner action -> Maybe (ExactOutcomeLaw (ExactStageOutcome owner state))
stochasticTransition (ExactStochasticGame _ _ _ _ rows) state profile = lookup (state, profile) rows

{- | A complete local mixed profile for each positive remaining horizon and
nonterminal public state.
-}
data ExactFiniteHorizonMarkovProfile owner state action
    = ExactFiniteHorizonMarkovProfile
        !(FiniteObject state)
        !(OwnedProduct owner action)
        !Natural
        ![((Natural, state), ExactMixedProfile owner action)]
    deriving (Eq, Show)

type role ExactFiniteHorizonMarkovProfile nominal nominal nominal

-- | Markov-profile construction failures.
data MarkovProfileError owner state
    = MarkovProfileHorizonLimitExceeded !Natural !Natural
    | ExcessMarkovProfileRows
    | DuplicateMarkovProfileRow !Natural !state
    | MissingMarkovProfileRow !Natural !state
    | MarkovProfileRowOutsideDomain !Natural !state
    | MarkovProfileLocalLayoutMismatch !Natural !state
    | MarkovProfileWorkLimitExceeded !Natural !Natural
    deriving (Eq, Show)

-- | Validate all local strategies.  Horizon zero requires no strategy rows.
exactFiniteHorizonMarkovProfile :: (Eq owner, Eq state, Eq action) => GameLimits -> ExactStochasticGame owner state action -> Horizon -> [((Natural, state), ExactMixedProfile owner action)] -> Either (MarkovProfileError owner state) (ExactFiniteHorizonMarkovProfile owner state action)
exactFiniteHorizonMarkovProfile limits game horizon supplied = do
    let count = horizonValue horizon
        states = NonEmpty.toList (finiteObjectValues (stochasticStates game))
        nonterminal = [state | state <- states, isNothing (stochasticTerminalValue game state)]
        work = cappedGameProduct (maximumGameWork limits) count (fromIntegral (length nonterminal))
    if count > maximumGameHorizon limits then Left (MarkovProfileHorizonLimitExceeded count (maximumGameHorizon limits)) else pure ()
    if work > maximumGameWork limits then Left (MarkovProfileWorkLimitExceeded work (maximumGameWork limits)) else pure ()
    let expected = [(remaining, state) | remaining <- [1 .. count], state <- nonterminal]
        bounded = take (length expected + 1) supplied
    if length bounded > length expected then Left ExcessMarkovProfileRows else pure ()
    case firstDuplicate (map fst bounded) of
        Just (remaining, state) -> Left (DuplicateMarkovProfileRow remaining state)
        Nothing -> pure ()
    case firstOutside expected (map fst bounded) of
        Just (remaining, state) -> Left (MarkovProfileRowOutsideDomain remaining state)
        Nothing -> pure ()
    rows <- traverse canonical expected
    Right (ExactFiniteHorizonMarkovProfile (stochasticStates game) (stochasticActions game) count rows)
  where
    canonical key@(remaining, state) = case lookup key supplied of
        Nothing -> Left (MissingMarkovProfileRow remaining state)
        Just mixed
            | mixedProfileProduct mixed /= stochasticActions game -> Left (MarkovProfileLocalLayoutMismatch remaining state)
            | otherwise -> case validateOwnedProduct limits (mixedProfileProduct mixed) of
                Left _ -> Left (MarkovProfileLocalLayoutMismatch remaining state)
                Right () -> case traverse validateRow (mixedProfileRows mixed) of
                    Left _ -> Left (MarkovProfileLocalLayoutMismatch remaining state)
                    Right _ -> Right (key, mixed)
    validateRow (_, simplex) = validateExactSimplex limits simplex

-- | Read represented horizon.
markovProfileHorizon :: ExactFiniteHorizonMarkovProfile owner state action -> Natural
markovProfileHorizon (ExactFiniteHorizonMarkovProfile _ _ horizon _) = horizon

-- | Query one local strategy.
markovStrategyAt :: (Eq state) => ExactFiniteHorizonMarkovProfile owner state action -> Natural -> state -> Maybe (ExactMixedProfile owner action)
markovStrategyAt (ExactFiniteHorizonMarkovProfile _ _ _ rows) remaining state = lookup (remaining, state) rows

-- | Dynamic evaluation failures.
data StochasticEvaluationError
    = StochasticGameProfileMismatch
    | StochasticInitialStateOutsideCarrier
    | StochasticWorkLimitExceeded !Natural !Natural
    | StochasticRationalLimitExceeded !String !Natural !Natural
    | StochasticInternalLayoutMismatch
    | StochasticLocalNashError !ExactEvaluationError
    deriving (Eq, Show)

{- | Exact dynamic-programming report.  Layers are ordered from horizon zero
through the requested horizon and states retain represented layout.
-}
data StochasticValueReport owner state = StochasticValueReport
    { stochasticEvaluatedHorizon :: !Natural
    , stochasticEvaluationWork :: !Natural
    , stochasticValueLayers :: ![(Natural, [(state, ExactPlayerValues owner)])]
    }
    deriving (Eq, Show)

-- | Evaluate a complete finite-horizon Markov profile.
evaluateMarkovProfile :: (Eq owner, Eq state, Eq action) => GameLimits -> ExactStochasticGame owner state action -> ExactFiniteHorizonMarkovProfile owner state action -> Either StochasticEvaluationError (StochasticValueReport owner state)
evaluateMarkovProfile limits game profile
    | not (profileMatches game profile) = Left StochasticGameProfileMismatch
    | work > maximumGameWork limits = Left (StochasticWorkLimitExceeded work (maximumGameWork limits))
    | otherwise = do
        validateStochasticInputs limits game profile
        layers <- buildLayers limits game profile
        Right (StochasticValueReport (markovProfileHorizon profile) work layers)
  where
    work = evaluationWork limits game (markovProfileHorizon profile)

-- | Query a report layer/state.
stochasticValueAt :: (Eq state) => StochasticValueReport owner state -> Natural -> state -> Maybe (ExactPlayerValues owner)
stochasticValueAt report remaining state = lookup remaining (stochasticValueLayers report) >>= lookup state

-- | One local continuation-game Nash check.
data MarkovPerfectCheck owner state action = MarkovPerfectCheck
    { markovPerfectRemainingHorizon :: !Natural
    , markovPerfectState :: !state
    , markovPerfectLocalReport :: !(MixedNashReport owner action)
    }
    deriving (Eq, Show)

-- | Deterministic finite-horizon Markov-perfect candidate report.
data MarkovPerfectReport owner state action = MarkovPerfectReport
    { markovPerfectSatisfied :: !Bool
    , markovPerfectSubgameCount :: !Natural
    , markovPerfectEvaluation :: !(StochasticValueReport owner state)
    , markovPerfectChecks :: ![MarkovPerfectCheck owner state action]
    }
    deriving (Eq, Show)

{- | Check the incumbent local mixture in every represented nonterminal
continuation game.  This is a candidate verifier, not a stationary solver.
-}
checkFiniteHorizonMarkovPerfect :: (Eq owner, Eq state, Eq action) => GameLimits -> ExactStochasticGame owner state action -> ExactFiniteHorizonMarkovProfile owner state action -> Either StochasticEvaluationError (MarkovPerfectReport owner state action)
checkFiniteHorizonMarkovPerfect limits game profile
    | not (profileMatches game profile) = Left StochasticGameProfileMismatch
    | totalWork > maximumGameWork limits = Left (StochasticWorkLimitExceeded totalWork (maximumGameWork limits))
    | otherwise = do
        validateStochasticInputs limits game profile
        evaluation <- evaluateMarkovProfile limits game profile
        checks <- traverse (checkSubgame evaluation) subgames
        Right
            MarkovPerfectReport
                { markovPerfectSatisfied = all (mixedNashSatisfied . markovPerfectLocalReport) checks
                , markovPerfectSubgameCount = fromIntegral (length checks)
                , markovPerfectEvaluation = evaluation
                , markovPerfectChecks = checks
                }
  where
    states = NonEmpty.toList (finiteObjectValues (stochasticStates game))
    nonterminal = [state | state <- states, isNothing (stochasticTerminalValue game state)]
    limit = maximumGameWork limits
    subgameCount = cappedGameProduct limit (markovProfileHorizon profile) (fromIntegral (length nonterminal))
    profileCount = fromIntegral (finiteObjectCardinality (ownedProfiles (stochasticActions game)))
    playerCount = fromIntegral (finiteObjectCardinality (stochasticPlayers game))
    deviationCount = foldl (cappedGameAdd limit) 0 [fromIntegral (finiteObjectCardinality choices) | (_, choices) <- ownedProductRows (stochasticActions game)]
    transitionOutcomeCount = foldl (cappedGameAdd limit) 0 [fromIntegral (length (outcomeEntries law)) | ((_, _), law) <- stochasticTransitionRows game]
    expectedSteps = cappedGameProduct limit profileCount (cappedGameAdd limit playerCount 1)
    deviationSteps = cappedGameProduct limit deviationCount (cappedGameProduct limit profileCount (cappedGameAdd limit playerCount 2))
    payoffSteps = cappedGameAdd limit (cappedGameProduct limit profileCount playerCount) (cappedGameProduct limit transitionOutcomeCount (cappedGameAdd limit playerCount 4))
    localWork = foldl (cappedGameAdd limit) 0 [expectedSteps, deviationSteps, payoffSteps]
    evaluationSteps = evaluationWork limits game (markovProfileHorizon profile)
    checkingSteps = cappedGameProduct limit subgameCount localWork
    totalWork = cappedGameAdd limit evaluationSteps checkingSteps
    subgames = [(remaining, state) | remaining <- [1 .. markovProfileHorizon profile], state <- nonterminal]
    checkSubgame evaluation (remaining, state) = do
        previous <- maybe (Left StochasticInternalLayoutMismatch) Right (lookup (remaining - 1) (stochasticValueLayers evaluation))
        payoffs <- traverse (continuationPayoff limits game previous state) (NonEmpty.toList (finiteObjectValues (ownedProfiles (stochasticActions game))))
        normal <- case exactNormalGame limits (stochasticActions game) payoffs of
            Left _ -> Left StochasticInternalLayoutMismatch
            Right checked -> Right checked
        local <- maybe (Left StochasticInternalLayoutMismatch) Right (markovStrategyAt profile remaining state)
        report <- case checkMixedNash limits normal local of
            Left problem -> Left (StochasticLocalNashError problem)
            Right checked -> Right checked
        Right (MarkovPerfectCheck remaining state report)

buildLayers :: (Eq owner, Eq state, Eq action) => GameLimits -> ExactStochasticGame owner state action -> ExactFiniteHorizonMarkovProfile owner state action -> Either StochasticEvaluationError [(Natural, [(state, ExactPlayerValues owner)])]
buildLayers limits game profile = go 1 [(0, base)] base
  where
    states = NonEmpty.toList (finiteObjectValues (stochasticStates game))
    base = [(state, fromMaybe (zeroPlayerValues (stochasticPlayers game)) (stochasticTerminalValue game state)) | state <- states]
    go remaining accumulated previous
        | remaining > markovProfileHorizon profile = Right accumulated
        | otherwise = do
            current <- traverse (stateValue remaining previous) states
            go (remaining + 1) (accumulated ++ [(remaining, current)]) current
    stateValue remaining previous state = case stochasticTerminalValue game state of
        Just terminal -> Right (state, terminal)
        Nothing -> do
            mixed <- maybe (Left StochasticInternalLayoutMismatch) Right (markovStrategyAt profile remaining state)
            payoffs <- traverse (continuationPayoff limits game previous state) (NonEmpty.toList (finiteObjectValues (ownedProfiles (stochasticActions game))))
            normal <- case exactNormalGame limits (stochasticActions game) payoffs of
                Left _ -> Left StochasticInternalLayoutMismatch
                Right checked -> Right checked
            value <- mapEvaluation (expectedUtility limits normal mixed)
            Right (state, value)

continuationPayoff :: (Eq owner, Eq state, Eq action) => GameLimits -> ExactStochasticGame owner state action -> [(state, ExactPlayerValues owner)] -> state -> OwnedProfile owner action -> Either StochasticEvaluationError (OwnedProfile owner action, ExactPlayerValues owner)
continuationPayoff limits game previous state actionProfile = do
    law <- maybe (Left StochasticInternalLayoutMismatch) Right (stochasticTransition game state actionProfile)
    value <- foldlM addAtom (zeroPlayerValues (stochasticPlayers game)) (outcomeEntries law)
    Right (actionProfile, value)
  where
    addAtom accumulator (outcome, mass) = do
        continuation <- maybe (Left StochasticInternalLayoutMismatch) Right (lookup (successorState outcome) previous)
        discounted <- mapEvaluation (scalePlayerValues limits (stochasticDiscount game) continuation)
        total <- mapEvaluation (addPlayerValues limits (stageRewards outcome) discounted)
        weighted <- mapEvaluation (scalePlayerValues limits mass total)
        mapEvaluation (addPlayerValues limits accumulator weighted)

profileMatches :: (Eq owner, Eq state, Eq action) => ExactStochasticGame owner state action -> ExactFiniteHorizonMarkovProfile owner state action -> Bool
profileMatches game (ExactFiniteHorizonMarkovProfile states product_ _ rows) =
    states == stochasticStates game
        && product_ == stochasticActions game
        && all ((== product_) . mixedProfileProduct . snd) rows

stochasticTransitionRows :: ExactStochasticGame owner state action -> [((state, OwnedProfile owner action), ExactOutcomeLaw (ExactStageOutcome owner state))]
stochasticTransitionRows (ExactStochasticGame _ _ _ _ transitions) = transitions

validateStochasticInputs :: (Eq owner, Eq action) => GameLimits -> ExactStochasticGame owner state action -> ExactFiniteHorizonMarkovProfile owner state action -> Either StochasticEvaluationError ()
validateStochasticInputs limits (ExactStochasticGame _ product_ discount terminals transitions) (ExactFiniteHorizonMarkovProfile _ profileProduct horizon rows)
    | product_ /= profileProduct = Left StochasticGameProfileMismatch
    | horizon > maximumGameHorizon limits = Left (StochasticWorkLimitExceeded (maximumGameWork limits + 1) (maximumGameWork limits))
    | otherwise = do
        case validateOwnedProduct limits product_ of
            Left _ -> Left StochasticGameProfileMismatch
            Right () -> pure ()
        _ <- checkedStochastic limits "discount" discount
        traverse_ validateTerminal terminals
        traverse_ validateTransition transitions
        traverse_ validateLocal rows
  where
    validateTerminal (_, Nothing) = Right ()
    validateTerminal (_, Just values) = traverse_ (\(_, value) -> checkedStochastic limits "terminal value" value >> Right ()) (playerValueEntries values)
    validateTransition (_, law) = do
        total <- foldlM addMass 0 (outcomeEntries law)
        if total == 1 then pure () else Left StochasticInternalLayoutMismatch
        traverse_ validateOutcome (outcomeEntries law)
    addMass accumulator (_, mass)
        | mass < 0 = Left StochasticInternalLayoutMismatch
        | otherwise = do
            _ <- checkedStochastic limits "transition mass" mass
            checkedStochastic limits "transition mass total" (accumulator + mass)
    validateOutcome (outcome, _) = traverse_ (\(_, reward) -> checkedStochastic limits "stage reward" reward >> Right ()) (playerValueEntries (stageRewards outcome))
    validateLocal (_, mixed) = traverse_ validateRow (mixedProfileRows mixed)
    validateRow (_, simplex) = case validateExactSimplex limits simplex of
        Left _ -> Left StochasticInternalLayoutMismatch
        Right () -> Right ()

checkedStochastic :: GameLimits -> String -> Rational -> Either StochasticEvaluationError Rational
checkedStochastic limits label value = case checkRationalSize limits value of
    Left (actual, maximum_) -> Left (StochasticRationalLimitExceeded label actual maximum_)
    Right valid -> Right valid

evaluationWork :: GameLimits -> ExactStochasticGame owner state action -> Natural -> Natural
evaluationWork limits (ExactStochasticGame states product_ _ terminal transitions) horizon =
    cappedGameProduct limit horizon (cappedGameProduct limit stateCount perState)
  where
    limit = maximumGameWork limits
    stateCount = fromIntegral (finiteObjectCardinality states)
    profileCount = fromIntegral (finiteObjectCardinality (ownedProfiles product_))
    playerCount = fromIntegral (finiteObjectCardinality (ownedOwners product_))
    outcomeCount = foldl (cappedGameAdd limit) 0 [fromIntegral (length (outcomeEntries law)) | (_, law) <- transitions]
    outcomeSteps = cappedGameProduct limit outcomeCount (cappedGameAdd limit playerCount 4)
    perState = cappedGameAdd limit profileCount outcomeSteps
    _ = terminal

mapEvaluation :: Either ExactEvaluationError value -> Either StochasticEvaluationError value
mapEvaluation result = case result of
    Left (EvaluationRationalLimitExceeded label actual maximum_) -> Left (StochasticRationalLimitExceeded label actual maximum_)
    Left (EvaluationWorkLimitExceeded required limit) -> Left (StochasticWorkLimitExceeded required limit)
    Left problem -> Left (StochasticLocalNashError problem)
    Right value -> Right value

foldlM :: (accumulator -> value -> Either error accumulator) -> accumulator -> [value] -> Either error accumulator
foldlM function = go
  where
    go accumulator [] = Right accumulator
    go accumulator (value : remaining) = case function accumulator value of
        Left problem -> Left problem
        Right next -> go next remaining

traverse_ :: (a -> Either error ()) -> [a] -> Either error ()
traverse_ function = foldlM (\() value -> function value) ()

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate [] = Nothing
firstDuplicate (value : remaining)
    | value `elem` remaining = Just value
    | otherwise = firstDuplicate remaining

firstOutside :: (Eq value) => [value] -> [value] -> Maybe value
firstOutside _ [] = Nothing
firstOutside allowed (value : remaining)
    | value `notElem` allowed = Just value
    | otherwise = firstOutside allowed remaining
