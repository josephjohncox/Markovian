module MixedBayesianGames (runMixedBayesianGameTests) where

import Control.Monad (replicateM)
import Data.List (find)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, isNothing)
import Markovian.Category.Finite.Object
import Markovian.Category.Finite.Set (finiteSet, finiteSetValues)
import Markovian.Category.Payoff.Exact (exactPayoff)
import Markovian.Game.Correlated.Exact
import Markovian.Game.Harsanyi.Exact
import Markovian.Game.NormalForm.Exact
import Markovian.Game.Open.Exact
import Markovian.Game.Open.Finite
import Markovian.Game.Open.Strategic.Exact
import Markovian.Game.Optic.Finite (applyFiniteFunction, finiteBudget)
import Markovian.Game.Outcome.Exact
import Markovian.Game.Profile.Finite
import Markovian.Game.Stochastic.Exact
import Markovian.Horizon

runMixedBayesianGameTests :: (String -> IO () -> IO ()) -> IO ()
runMixedBayesianGameTests run = do
    run "owned products and exact simplexes enforce complete bounded tables" testProfileFoundation
    run "matching pennies has an exact half-half mixed Nash and product CE" testMatchingPennies
    run "all binary two-player games match independent Nash and CE oracles" testMixedDifferential
    run "Dirac CE is equivalent to pure Nash on every binary payoff table" testDiracCeIffPureNash
    run "independence rejects mismatched and zero-mass subcarriers" testIndependentCarrierMismatch
    run "Battle of the Sexes correlation is distinct from product marginals" testCorrelationNotProduct
    run "a coarse correlated equilibrium can fail conditional obedience" testCceNotCe
    run "a rational three-player candidate exposes the irrational-equilibrium boundary" testIrrationalBoundary
    run "joint outcome laws retain reward/successor correlation" testOutcomeCorrelation
    run "finite-horizon stochastic evaluation preserves terminal and horizon timing" testStochasticTiming
    run "Harsanyi checks retain correlated priors and report null types" testHarsanyi
    run "direct and converted values are sensitive to the correlated prior" testCorrelatedPriorSensitivity
    run "strategic-normal conversion agrees with direct one-shot type evaluation" testStrategicNormal
    run "a checked closed open-game context extracts one exact normal form" testOpenStrategicBridge
    run "degenerate zero-payoff games retain multiple rational equilibria" testDegenerateBoundary
    run "active limits revalidate stored payoffs and contingent-plan powers" testActiveLimitsAndPlanPreflight
    run "mixed-game reports match a golden and fail atomically at one-below work" testDeterminismAndBounds

largeLimits :: GameLimits
largeLimits = gameLimits 32 32 200000 1000000 10000000 4096 32

data Player = Row | Column | Third
    deriving (Eq, Ord, Show)

data Action = A | B | C | L | R | Safe | Risk
    deriving (Eq, Ord, Show)

data PublicState = Start | Win | Lose
    deriving (Eq, Ord, Show)

data PrivateType = T0 | T1 | TNull
    deriving (Eq, Ord, Show)

object :: (Eq value, Show value) => [value] -> IO (FiniteObject value)
object represented = right "finite object" (finiteObject represented)

right :: (Show error) => String -> Either error value -> IO value
right _ (Right value) = pure value
right label (Left problem) = ioError (userError (label ++ ": " ++ show problem))

assert :: String -> Bool -> IO ()
assert message condition = if condition then pure () else ioError (userError message)

traverse_ :: (a -> IO ()) -> [a] -> IO ()
traverse_ = mapM_

expectLeft :: String -> (error -> Bool) -> Either error value -> IO ()
expectLeft label predicate result = case result of
    Left problem -> assert (label ++ ": wrong error") (predicate problem)
    Right _ -> ioError (userError (label ++ ": unexpectedly succeeded"))

productFor :: [(Player, [Action])] -> IO (OwnedProduct Player Action)
productFor rows = do
    owners <- object (map fst rows)
    checkedRows <- traverse (\(owner, choices) -> (owner,) <$> object choices) rows
    right "owned product" (ownedProduct largeLimits owners checkedRows)

values :: FiniteObject Player -> [(Player, Rational)] -> IO (ExactPlayerValues Player)
values owners entries = right "player values" (exactPlayerValues largeLimits owners entries)

simplex :: FiniteObject Action -> [(Action, Rational)] -> IO (ExactSimplex Action)
simplex carrier entries = right "simplex" (exactSimplex largeLimits carrier entries)

profileAt :: OwnedProduct Player Action -> [(Player, Action)] -> IO (OwnedProfile Player Action)
profileAt product_ entries = right "profile" (ownedProfile product_ entries)

normalGame :: [(Player, [Action])] -> (Player -> [(Player, Action)] -> Rational) -> IO (ExactNormalGame Player Action)
normalGame rows payoff = do
    product_ <- productFor rows
    let owners = ownedOwners product_
        profiles = NonEmpty.toList (finiteObjectValues (ownedProfiles product_))
    table <- traverse (\profile -> (profile,) <$> values owners [(owner, payoff owner (ownedProfileEntries profile)) | owner <- NonEmpty.toList (finiteObjectValues owners)]) profiles
    right "normal game" (exactNormalGame largeLimits product_ table)

mixedRows :: OwnedProduct Player Action -> [(Player, [(Action, Rational)])] -> IO (ExactMixedProfile Player Action)
mixedRows product_ rows = do
    checked <- traverse build rows
    right "mixed profile" (exactMixedProfile largeLimits product_ checked)
  where
    build (owner, masses) = case ownedChoices product_ owner of
        Nothing -> ioError (userError "outside owner")
        Just carrier -> (owner,) <$> simplex carrier masses

deviceFrom :: OwnedProduct Player Action -> (OwnedProfile Player Action -> Rational) -> IO (ExactCorrelationDevice Player Action)
deviceFrom product_ mass =
    right "correlation device" (exactCorrelationDevice largeLimits product_ [(profile, mass profile) | profile <- NonEmpty.toList (finiteObjectValues (ownedProfiles product_))])

lookupChoice :: (Eq owner) => owner -> [(owner, choice)] -> choice
lookupChoice owner entries = case lookup owner entries of
    Just choice -> choice
    Nothing -> error "fixture profile is complete"

testProfileFoundation :: IO ()
testProfileFoundation = do
    owners <- object [Row, Column]
    rowActions <- object [A, B]
    columnActions <- object [L, R]
    product_ <- right "heterogeneous product" (ownedProduct largeLimits owners [(Column, columnActions), (Row, rowActions)])
    assert "profile count" (finiteObjectCardinality (ownedProfiles product_) == 4)
    let first = NonEmpty.head (finiteObjectValues (ownedProfiles product_))
    assert "canonical owner order" (map fst (ownedProfileEntries first) == [Row, Column])
    expectLeft "missing simplex row" missing (exactSimplex largeLimits rowActions [(A, 1)])
    expectLeft "not normalized" notOne (exactSimplex largeLimits rowActions [(A, 1), (B, 1)])
    expectLeft "infinite simplex input" (== ExcessSimplexEntries) (exactSimplex largeLimits rowActions (cycle [(A, 1), (B, 0)]))
    rowSimplex <- simplex rowActions [(A, 1), (B, 0)]
    columnSimplex <- simplex columnActions [(L, 1), (R, 0)]
    expectLeft "infinite mixed rows" (== ExcessMixedProfileRows) (exactMixedProfile largeLimits product_ (cycle [(Row, rowSimplex), (Column, columnSimplex)]))
    expectLeft "product preflight" productLimit (ownedProduct (gameLimits 2 2 3 100 100 100 1) owners [(Row, rowActions), (Column, columnActions)])
  where
    missing (MissingSimplexChoice B) = True
    missing _ = False
    notOne (SimplexMassNotOne 2) = True
    notOne _ = False
    productLimit (ProductCardinalityLimitExceeded 4 3) = True
    productLimit _ = False

testMatchingPennies :: IO ()
testMatchingPennies = do
    game <- normalGame [(Row, [A, B]), (Column, [A, B])] payoff
    let product_ = normalGameProduct game
    mixed <- mixedRows product_ [(Row, [(A, 1 / 2), (B, 1 / 2)]), (Column, [(A, 1 / 2), (B, 1 / 2)])]
    report <- right "mixed Nash" (checkMixedNash largeLimits game mixed)
    assert "matching pennies mixed Nash" (mixedNashSatisfied report)
    device <- deviceFrom product_ (either (const 0) (fromMaybe 0) . mixedProfileProbability largeLimits mixed)
    ce <- right "product CE" (checkCorrelatedEquilibrium largeLimits game device)
    independent <- right "independence" (isIndependentCorrelation largeLimits mixed device)
    assert "mixed Nash did not induce CE" (correlatedEquilibriumSatisfied ce && independent)
  where
    payoff Row entries = if lookupChoice Row entries == lookupChoice Column entries then 1 else -1
    payoff Column entries = negate (payoff Row entries)
    payoff Third _ = 0

testMixedDifferential :: IO ()
testMixedDifferential = traverse_ checkTable (replicateM 8 [0, 1 :: Rational])
  where
    probabilities = [0, 1 / 2, 1]
    checkTable table = do
        game <- normalGame [(Row, [A, B]), (Column, [A, B])] (tablePayoff table)
        traverse_ (checkCandidate game table) [(p, q) | p <- probabilities, q <- probabilities]
    checkCandidate game table (p, q) = do
        let product_ = normalGameProduct game
        mixed <- mixedRows product_ [(Row, [(A, p), (B, 1 - p)]), (Column, [(A, q), (B, 1 - q)])]
        report <- right "binary differential" (checkMixedNash largeLimits game mixed)
        assert "independent mixed-Nash differential" (mixedNashSatisfied report == rawNash table p q)
        device <- deviceFrom product_ (either (const 0) (fromMaybe 0) . mixedProfileProbability largeLimits mixed)
        ce <- right "product CE differential" (checkCorrelatedEquilibrium largeLimits game device)
        assert "product Nash must imply CE" (not (mixedNashSatisfied report) || correlatedEquilibriumSatisfied ce)
    tablePayoff table owner entries = table !! (ownerOffset owner + profileIndex entries)
    ownerOffset Row = 0
    ownerOffset Column = 4
    ownerOffset Third = 0
    profileIndex entries = case (lookupChoice Row entries, lookupChoice Column entries) of
        (A, A) -> 0
        (A, B) -> 1
        (B, A) -> 2
        (B, B) -> 3
        _ -> error "binary profile invariant"
    rawNash table p q =
        let at index = table !! index
            rowA = q * at 0 + (1 - q) * at 1
            rowB = q * at 2 + (1 - q) * at 3
            rowIncumbent = p * rowA + (1 - p) * rowB
            columnA = p * at 4 + (1 - p) * at 6
            columnB = p * at 5 + (1 - p) * at 7
            columnIncumbent = q * columnA + (1 - q) * columnB
         in rowIncumbent >= rowA && rowIncumbent >= rowB && columnIncumbent >= columnA && columnIncumbent >= columnB

testDiracCeIffPureNash :: IO ()
testDiracCeIffPureNash = traverse_ checkTable (replicateM 8 [0, 1 :: Rational])
  where
    checkTable table = do
        game <- normalGame [(Row, [A, B]), (Column, [A, B])] payoffFor
        traverse_ (checkProfile game) (NonEmpty.toList (finiteObjectValues (ownedProfiles (normalGameProduct game))))
      where
        payoffFor owner entries = table !! (if owner == Row then profileIndex entries else 4 + profileIndex entries)
    profileIndex entries = case (lookupChoice Row entries, lookupChoice Column entries) of
        (A, A) -> 0
        (A, B) -> 1
        (B, A) -> 2
        (B, B) -> 3
        _ -> error "binary profile invariant"
    checkProfile game profile = do
        let product_ = normalGameProduct game
        mixed <- right "Dirac mixed profile" (diracMixedProfile largeLimits product_ profile)
        nash <- right "Dirac Nash" (checkMixedNash largeLimits game mixed)
        device <- deviceFrom product_ (\candidate -> if candidate == profile then 1 else 0)
        ce <- right "Dirac CE" (checkCorrelatedEquilibrium largeLimits game device)
        assert "Dirac CE iff pure Nash" (correlatedEquilibriumSatisfied ce == mixedNashSatisfied nash)

testIndependentCarrierMismatch :: IO ()
testIndependentCarrierMismatch = do
    owners <- object [Row, Column]
    wide <- object [A, B]
    narrow <- object [A]
    wideProduct <- right "wide product" (ownedProduct largeLimits owners [(Row, wide), (Column, wide)])
    narrowProduct <- right "narrow product" (ownedProduct largeLimits owners [(Row, narrow), (Column, narrow)])
    wideDirac <- simplex wide [(A, 1), (B, 0)]
    mixed <- right "wide mixed" (exactMixedProfile largeLimits wideProduct [(Row, wideDirac), (Column, wideDirac)])
    narrowDevice <- deviceFrom narrowProduct (const 1)
    expectLeft "zero-mass subcarrier mismatch" (== CorrelatedGameDeviceMismatch) (isIndependentCorrelation largeLimits mixed narrowDevice)
    reversed <- object [B, A]
    reorderedProduct <- right "reordered product" (ownedProduct largeLimits owners [(Row, reversed), (Column, reversed)])
    reorderedDirac <- simplex reversed [(B, 0), (A, 1)]
    reorderedMixed <- right "reordered mixed" (exactMixedProfile largeLimits reorderedProduct [(Row, reorderedDirac), (Column, reorderedDirac)])
    wideDevice <- deviceFrom wideProduct (\profile -> if all ((== A) . snd) (ownedProfileEntries profile) then 1 else 0)
    expectLeft "layout mismatch" (== CorrelatedGameDeviceMismatch) (isIndependentCorrelation largeLimits reorderedMixed wideDevice)

testCorrelationNotProduct :: IO ()
testCorrelationNotProduct = do
    game <- normalGame [(Row, [A, B]), (Column, [A, B])] payoff
    let product_ = normalGameProduct game
        coordinated profile =
            let entries = ownedProfileEntries profile
             in if lookupChoice Row entries == lookupChoice Column entries then 1 / 2 else 0
    device <- deviceFrom product_ coordinated
    ce <- right "Battle of the Sexes CE" (checkCorrelatedEquilibrium largeLimits game device)
    marginals <- mixedRows product_ [(Row, [(A, 1 / 2), (B, 1 / 2)]), (Column, [(A, 1 / 2), (B, 1 / 2)])]
    independent <- right "nonproduct check" (isIndependentCorrelation largeLimits marginals device)
    assert "coordination device was not CE" (correlatedEquilibriumSatisfied ce)
    assert "correlation collapsed to product" (not independent)
  where
    payoff owner entries = case (lookupChoice Row entries, lookupChoice Column entries, owner) of
        (A, A, Row) -> 2
        (A, A, Column) -> 1
        (B, B, Row) -> 1
        (B, B, Column) -> 2
        _ -> 0

testCceNotCe :: IO ()
testCceNotCe = do
    game <- normalGame [(Row, [A, B, C]), (Column, [L, R])] payoff
    let product_ = normalGameProduct game
        mass profile = case (lookupChoice Row entries, lookupChoice Column entries) of
            (A, L) -> 1 / 2
            (C, R) -> 1 / 2
            _ -> 0
          where
            entries = ownedProfileEntries profile
    device <- deviceFrom product_ mass
    cce <- right "CCE" (checkCoarseCorrelatedEquilibrium largeLimits game device)
    ce <- right "CE" (checkCorrelatedEquilibrium largeLimits game device)
    assert "fixture should be CCE" (coarseCorrelatedEquilibriumSatisfied cce)
    assert "fixture should not be CE" (not (correlatedEquilibriumSatisfied ce))
    assert "null recommendations were not explicit" (any ((== NullRecommendation) . recommendationStatus) (correlatedObedienceChecks ce))
    assert "missing profitable conditional deviation" (any ((< 0) . obedienceSlack) (correlatedObedienceChecks ce))
  where
    payoff Row entries = case (lookupChoice Row entries, lookupChoice Column entries) of
        (B, L) -> 1
        (B, R) -> -1
        _ -> 0
    payoff _ _ = 0

testIrrationalBoundary :: IO ()
testIrrationalBoundary = do
    game <- normalGame [(Row, [A, B]), (Column, [A, B]), (Third, [A, B])] payoff
    let product_ = normalGameProduct game
        half = [(A, 1 / 2), (B, 1 / 2)]
    mixed <- mixedRows product_ [(Row, half), (Column, half), (Third, half)]
    report <- right "three-player candidate" (checkMixedNash largeLimits game mixed)
    assert "half candidate unexpectedly solved p^2=1/2" (not (mixedNashSatisfied report))
    assert "expected profitable B deviation" (any ((> 0) . deviationGain) (mixedNashDeviations report))
  where
    payoff owner entries
        | lookupChoice owner entries == B = 0
        | all (== A) [choice | (other, choice) <- entries, other /= owner] = 1 / 2
        | otherwise = -(1 / 2)

testOutcomeCorrelation :: IO ()
testOutcomeCorrelation = do
    carrier <- object [(1 :: Rational, Win), (0, Lose)]
    law <- right "joint law" (exactOutcomeLaw largeLimits carrier [((1, Win), 1 / 2), ((0, Lose), 1 / 2)])
    joint <- right "joint moment" (outcomeExpectation largeLimits law (\(reward, state) -> reward * if state == Win then 1 else 0))
    let rewardMean = 1 / 2 :: Rational
        winMean = 1 / 2 :: Rational
    assert "joint law was replaced by marginals" (joint == 1 / 2 && rewardMean * winMean == 1 / 4)

testStochasticTiming :: IO ()
testStochasticTiming = do
    owners <- object [Row]
    states <- object [Start, Win, Lose]
    actions <- object [Safe, Risk]
    product_ <- right "stochastic actions" (ownedProduct largeLimits owners [(Row, actions)])
    zero <- values owners [(Row, 0)]
    one <- values owners [(Row, 1)]
    safeProfile <- profileAt product_ [(Row, Safe)]
    riskProfile <- profileAt product_ [(Row, Risk)]
    safeOutcomeCarrier <- object [exactStageOutcome zero Win]
    safeLaw <- right "safe law" (exactOutcomeLaw largeLimits safeOutcomeCarrier [(exactStageOutcome zero Win, 1)])
    riskOutcomeCarrier <- object [exactStageOutcome one Lose, exactStageOutcome zero Win]
    riskLaw <- right "risk law" (exactOutcomeLaw largeLimits riskOutcomeCarrier [(exactStageOutcome one Lose, 1 / 2), (exactStageOutcome zero Win, 1 / 2)])
    game <- right "stochastic game" (exactStochasticGame largeLimits states product_ 1 [(Start, Nothing), (Win, Just one), (Lose, Just zero)] [((Start, safeProfile), safeLaw), ((Start, riskProfile), riskLaw)])
    horizon0 <- right "horizon zero" (mkHorizon 0)
    profile0 <- right "zero profile" (exactFiniteHorizonMarkovProfile largeLimits game horizon0 [])
    report0 <- right "zero evaluation" (evaluateMarkovProfile largeLimits game profile0)
    assert "horizon-zero nonterminal value" ((stochasticValueAt report0 0 Start >>= (`playerValue` Row)) == Just 0)
    assert "terminal value was hidden at horizon zero" ((stochasticValueAt report0 0 Win >>= (`playerValue` Row)) == Just 1)
    riskSimplex <- simplex actions [(Safe, 0), (Risk, 1)]
    local <- right "local mix" (exactMixedProfile largeLimits product_ [(Row, riskSimplex)])
    horizon1 <- right "horizon one" (mkHorizon 1)
    profile1 <- right "one profile" (exactFiniteHorizonMarkovProfile largeLimits game horizon1 [((1, Start), local)])
    report1 <- right "one evaluation" (evaluateMarkovProfile largeLimits game profile1)
    assert "joint reward and terminal continuation timing" ((stochasticValueAt report1 1 Start >>= (`playerValue` Row)) == Just 1)
    mpe <- right "Markov perfect check" (checkFiniteHorizonMarkovPerfect largeLimits game profile1)
    assert "one-stage tie should be Markov perfect" (markovPerfectSatisfied mpe)

harsanyiFixture :: IO (ExactHarsanyiGame Player PrivateType Action, ExactBehaviorProfile Player PrivateType Action)
harsanyiFixture = do
    owners <- object [Row, Column]
    types <- object [T0, T1, TNull]
    actions <- object [A, B]
    typeProduct <- right "type product" (ownedProduct largeLimits owners [(Row, types), (Column, types)])
    actionProduct <- right "Bayesian action product" (ownedProduct largeLimits owners [(Row, actions), (Column, actions)])
    let typeProfiles = NonEmpty.toList (finiteObjectValues (ownedProfiles typeProduct))
        priorMass profile = case (lookupChoice Row entries, lookupChoice Column entries) of
            (T0, T0) -> 1 / 2
            (T1, T1) -> 1 / 2
            _ -> 0
          where
            entries = ownedProfileEntries profile
    prior <- right "correlated common prior" (exactTypePrior largeLimits typeProduct [(profile, priorMass profile) | profile <- typeProfiles])
    behaviorRows <- traverse (behaviorRow actions) [(owner, typ) | owner <- [Row, Column], typ <- [T0, T1, TNull]]
    behavior <- right "behavior" (exactBehaviorProfile largeLimits typeProduct actionProduct behaviorRows)
    let actionProfiles = NonEmpty.toList (finiteObjectValues (ownedProfiles actionProduct))
    payoffTable <- traverse (payoffEntry owners) [(typeProfile, actionProfile) | typeProfile <- typeProfiles, actionProfile <- actionProfiles]
    game <- right "Harsanyi game" (exactHarsanyiGame largeLimits typeProduct actionProduct prior payoffTable)
    pure (game, behavior)
  where
    behaviorRow carrier (owner, typ) = do
        let selected = if typ == T1 then B else A
        strategy <- simplex carrier [(action, if action == selected then 1 else 0) | action <- [A, B]]
        pure ((owner, typ), strategy)
    payoffEntry owners (typeProfile, actionProfile) = do
        utility <- values owners [(owner, utilityFor owner) | owner <- [Row, Column]]
        pure ((typeProfile, actionProfile), utility)
      where
        utilityFor owner = case profileChoice typeProfile owner of
            Just T0 -> if profileChoice actionProfile owner == Just A then 1 else 0
            Just T1 -> if profileChoice actionProfile owner == Just B then 1 else 0
            Just TNull -> 0
            Nothing -> 0

testHarsanyi :: IO ()
testHarsanyi = do
    (game, behavior) <- harsanyiFixture
    interim <- right "interim Bayes Nash" (checkPositiveTypeInterimBayesNash largeLimits game behavior)
    exAnte <- right "ex-ante Bayes Nash" (checkExAnteBayesNash largeLimits game behavior)
    assert "truthful behavior should be Bayes Nash" (positiveTypeBayesNashSatisfied interim && exAnteBayesNashSatisfied exAnte)
    let nullRows = [row | row <- bayesDeviations interim, observedOwnType row == TNull]
    assert "null types were not explicit" (not (null nullRows) && all ((== NullPriorType) . bayesTypeStatus) nullRows)
    assert "null posterior was invented" (all (isNothing . conditionalIncumbentPayoff) nullRows)

priorSensitiveFixture :: Bool -> IO (ExactHarsanyiGame Player PrivateType Action, ExactBehaviorProfile Player PrivateType Action)
priorSensitiveFixture diagonal = do
    owners <- object [Row, Column]
    types <- object [T0, T1]
    actions <- object [A, B]
    typeProduct <- right "sensitive type product" (ownedProduct largeLimits owners [(Row, types), (Column, types)])
    actionProduct <- right "sensitive action product" (ownedProduct largeLimits owners [(Row, actions), (Column, actions)])
    let typeProfiles = NonEmpty.toList (finiteObjectValues (ownedProfiles typeProduct))
        priorMass profile =
            let same = profileChoice profile Row == profileChoice profile Column
             in if same == diagonal then 1 / 2 else 0
    prior <- right "sensitive prior" (exactTypePrior largeLimits typeProduct [(profile, priorMass profile) | profile <- typeProfiles])
    behaviorRows <- traverse behaviorRow [(owner, typ) | owner <- [Row, Column], typ <- [T0, T1]]
    behavior <- right "sensitive behavior" (exactBehaviorProfile largeLimits typeProduct actionProduct behaviorRows)
    let actionProfiles = NonEmpty.toList (finiteObjectValues (ownedProfiles actionProduct))
    payoffs <- traverse (payoffEntry owners) [(typeProfile, actionProfile) | typeProfile <- typeProfiles, actionProfile <- actionProfiles]
    game <- right "sensitive game" (exactHarsanyiGame largeLimits typeProduct actionProduct prior payoffs)
    pure (game, behavior)
  where
    behaviorRow (owner, typ) = do
        actionCarrier <- object [A, B]
        strategy <- simplex actionCarrier [(action, if action == actionFor typ then 1 else 0) | action <- [A, B]]
        pure ((owner, typ), strategy)
    payoffEntry owners (typeProfile, actionProfile) = do
        utility <- values owners [(owner, if profileChoice actionProfile owner == (actionFor <$> profileChoice typeProfile (other owner)) then 1 else 0) | owner <- [Row, Column]]
        pure ((typeProfile, actionProfile), utility)
    actionFor T0 = A
    actionFor _ = B
    other Row = Column
    other _ = Row

testCorrelatedPriorSensitivity :: IO ()
testCorrelatedPriorSensitivity = do
    (diagonalGame, diagonalBehavior) <- priorSensitiveFixture True
    (antiGame, antiBehavior) <- priorSensitiveFixture False
    diagonalDirect <- right "diagonal direct" (checkPositiveTypeInterimBayesNash largeLimits diagonalGame diagonalBehavior)
    antiDirect <- right "anti direct" (checkPositiveTypeInterimBayesNash largeLimits antiGame antiBehavior)
    assert "direct checker ignored correlated prior" (positiveTypeBayesNashSatisfied diagonalDirect && not (positiveTypeBayesNashSatisfied antiDirect))
    diagonalConverted <- right "diagonal converted" (toStrategicNormalForm largeLimits diagonalGame)
    antiConverted <- right "anti converted" (toStrategicNormalForm largeLimits antiGame)
    diagonalPayoff <- truthfulPayoff diagonalConverted
    antiPayoff <- truthfulPayoff antiConverted
    assert "strategic conversion ignored correlated prior" (diagonalPayoff == 1 && antiPayoff == 0)
  where
    truthfulPayoff converted = case find truthful (NonEmpty.toList (finiteObjectValues (ownedProfiles (normalGameProduct converted)))) of
        Nothing -> ioError (userError "truthful sensitive plan absent")
        Just profile -> maybe (ioError (userError "truthful payoff absent")) pure (normalPayoff converted Row profile)
    truthful profile = case profileChoice profile Row of
        Nothing -> False
        Just plan -> lookup T0 (contingentPlanEntries plan) == Just A && lookup T1 (contingentPlanEntries plan) == Just B

testStrategicNormal :: IO ()
testStrategicNormal = do
    (game, _) <- harsanyiFixture
    converted <- right "strategic normal" (toStrategicNormalForm largeLimits game)
    let profiles = NonEmpty.toList (finiteObjectValues (ownedProfiles (normalGameProduct converted)))
        truthful plan owner = case profileChoice plan owner of
            Nothing -> False
            Just contingent -> lookup T0 (contingentPlanEntries contingent) == Just A && lookup T1 (contingentPlanEntries contingent) == Just B
        candidate = find (\plan -> truthful plan Row && truthful plan Column) profiles
    case candidate of
        Nothing -> ioError (userError "truthful contingent plan absent")
        Just plan -> do
            assert "converted row payoff" (normalPayoff converted Row plan == Just 1)
            assert "converted column payoff" (normalPayoff converted Column plan == Just 1)

testOpenStrategicBridge :: IO ()
testOpenStrategicBridge = do
    unit <- right "unit finite set" (finiteSet [()])
    actionSet <- right "action finite set" (finiteSet [A, B])
    utilitySet <- right "utility finite set" (finiteSet [0, 1 :: Rational])
    open <- right "exact open decision" (exactMaximizingDecision (finiteBudget 1000) Row unit actionSet utilitySet)
    payoff <- right "open payoff" (exactPayoff actionSet [(A, 1), (B, 0)])
    context <- right "open context" (contextFromExactPayoff (finiteBudget 1000) open () payoff)
    localProduct <- productFor [(Row, [A, B])]
    let globals = finiteSetValues (strategySchemaProfiles (openGameStrategySchema open))
        globalFor action = find (\strategy -> applyFiniteFunction strategy () == Just action) globals
    mappings <-
        traverse
            ( \profile -> case profileChoice profile Row >>= globalFor of
                Nothing -> ioError (userError "global strategy missing")
                Just strategy -> pure (profile, strategy)
            )
            (NonEmpty.toList (finiteObjectValues (ownedProfiles localProduct)))
    layout <- right "deviation layout" (ownedDeviationLayout largeLimits localProduct (openGameStrategySchema open) mappings)
    extracted <- right "strategic extraction" (normalFormFromOpenContext largeLimits open context layout (\_ utility -> utility))
    chosen <- profileAt localProduct [(Row, A)]
    rejected <- profileAt localProduct [(Row, B)]
    assert "extracted maximizing payoff" (normalPayoff extracted Row chosen == Just 1)
    assert "extracted losing payoff" (normalPayoff extracted Row rejected == Just 0)

testDegenerateBoundary :: IO ()
testDegenerateBoundary = do
    game <- normalGame [(Row, [A, B]), (Column, [A, B])] (\_ _ -> 0)
    let product_ = normalGameProduct game
    pureA <- mixedRows product_ [(Row, [(A, 1), (B, 0)]), (Column, [(A, 1), (B, 0)])]
    uniform <- mixedRows product_ [(Row, [(A, 1 / 2), (B, 1 / 2)]), (Column, [(A, 1 / 2), (B, 1 / 2)])]
    first <- right "degenerate pure" (checkMixedNash largeLimits game pureA)
    second <- right "degenerate uniform" (checkMixedNash largeLimits game uniform)
    assert "zero game should have many equilibrium candidates" (mixedNashSatisfied first && mixedNashSatisfied second && pureA /= uniform)

testActiveLimitsAndPlanPreflight :: IO ()
testActiveLimitsAndPlanPreflight = do
    game <- normalGame [(Row, [A, B]), (Column, [A, B])] payoff
    mixed <- mixedRows (normalGameProduct game) [(Row, [(A, 1), (B, 0)]), (Column, [(A, 1), (B, 0)])]
    let tightRational = gameLimits 32 32 200000 1000000 10000000 2 32
    expectLeft "stored payoff active rational limit" rationalFailure (expectedUtility tightRational game mixed)
    (harsanyi, _) <- harsanyiFixture
    let planBelow = gameLimits 32 7 200000 1000000 10000000 4096 32
    expectLeft "contingent-plan one below" planFailure (toStrategicNormalForm planBelow harsanyi)
  where
    payoff Row entries = if lookupChoice Row entries == A && lookupChoice Column entries == A then 1 / 3 else 0
    payoff _ _ = 0
    rationalFailure (EvaluationRationalLimitExceeded _ actual maximum_) = actual > maximum_
    rationalFailure _ = False
    planFailure (StrategicNormalPlanCountLimitExceeded _ required limit) = required == limit + 1
    planFailure _ = False

testDeterminismAndBounds :: IO ()
testDeterminismAndBounds = do
    game <- normalGame [(Row, [A, B]), (Column, [A, B])] (\_ _ -> 0)
    let product_ = normalGameProduct game
    mixed <- mixedRows product_ [(Row, [(A, 1 / 2), (B, 1 / 2)]), (Column, [(A, 1 / 2), (B, 1 / 2)])]
    first <- right "first report" (checkMixedNash largeLimits game mixed)
    golden <- readFile "test/golden/mixed-nash-zero.txt"
    assert "mixed-Nash report changed from golden" (show first ++ "\n" == golden)
    let required = mixedNashArithmeticWork first
        below = gameLimits 32 32 200000 1000000 (required - 1) 4096 32
    expectLeft "one below" exactWork (checkMixedNash below game mixed)
  where
    exactWork (EvaluationWorkLimitExceeded required limit) = required == limit + 1
    exactWork _ = False
