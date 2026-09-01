{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}

module CircuitCostRewrite (runCircuitCostRewriteTests) where

import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Markovian.Algebra.NonNegativeRational
import Markovian.Algebra.Semiring (zero)
import Markovian.Category.Finite.Set
import Markovian.Category.Matrix
import Markovian.Category.Matrix.Deterministic
import Markovian.Category.Matrix.Stochastic
import Markovian.Circuit
import Markovian.Circuit.Compile.Deterministic
import Markovian.Circuit.Interpret.Cost
import Markovian.Circuit.Interpret.Exact
import Markovian.Circuit.Rewrite.Deterministic
import Markovian.Circuit.Rewrite.Deterministic.Exact
import Numeric.Natural (Natural)

runCircuitCostRewriteTests :: (String -> IO () -> IO ()) -> IO ()
runCircuitCostRewriteTests run = do
    run "bounded circuit cost reports and elaborated sharing" testCostReports
    run "constructor-complete bounded-fold cost differential" testConstructorCostDifferential
    run "circuit cost limits and callback ownership" testCostLimits
    run "exact deterministic rewrite certificates" testRewriteCertificates
    run "deterministic copy rewrite counterexamples" testRewriteCounterexamples

data CostPrimitive purity source target where
    NotPrimitive :: CostPrimitive 'Deterministic Bool Bool
    BadPrimitive :: CostPrimitive 'Deterministic Bool Bool
    FairCoinPrimitive :: CostPrimitive 'Stochastic () Bool
    DiracStochasticPrimitive :: CostPrimitive 'Stochastic Bool Bool

data CostError = CostCallbackFailure | ExactPrimitiveFailure
    deriving (Eq, Show)

data Owner = LogicOwner | RandomOwner | TableOwner
    deriving (Eq, Show)

costInterpreter :: CircuitCostInterpreter CostPrimitive Owner CostError
costInterpreter =
    CircuitCostInterpreter
        { costDeterministicPrimitive = \_ _ primitive ->
            case primitive of
                NotPrimitive -> Right (primitiveCharge LogicOwner 7)
                BadPrimitive -> Left CostCallbackFailure
        , costStochasticPrimitive = \_ _ primitive ->
            case primitive of
                FairCoinPrimitive -> Right (primitiveCharge RandomOwner 11)
                DiracStochasticPrimitive -> Right (primitiveCharge RandomOwner 5)
        , costDeterministicTable = \_ -> Right (primitiveCharge TableOwner 3)
        }

zeroCostInterpreter :: CircuitCostInterpreter CostPrimitive Owner CostError
zeroCostInterpreter =
    CircuitCostInterpreter
        { costDeterministicPrimitive = \_ _ _ -> Right (primitiveCharge LogicOwner 0)
        , costStochasticPrimitive = \_ _ _ -> Right (primitiveCharge RandomOwner 0)
        , costDeterministicTable = \_ -> Right (primitiveCharge TableOwner 0)
        }

exactInterpreter :: ExactPrimitiveInterpreter CostPrimitive CostError
exactInterpreter =
    ExactPrimitiveInterpreter
        { interpretDeterministicPrimitive = \source target primitive ->
            case primitive of
                NotPrimitive -> mapDeterministic (deterministicFromFunction source target not)
                BadPrimitive -> Left ExactPrimitiveFailure
        , interpretStochasticPrimitive = \source target primitive ->
            case primitive of
                FairCoinPrimitive -> fairCoin source target
                DiracStochasticPrimitive ->
                    case deterministicFromFunction source target id of
                        Left _ -> Left ExactPrimitiveFailure
                        Right arrow -> Right (embedDeterministic arrow)
        }

reorderedExactInterpreter :: ExactPrimitiveInterpreter CostPrimitive CostError
reorderedExactInterpreter =
    ExactPrimitiveInterpreter
        { interpretDeterministicPrimitive = \_ target primitive ->
            case primitive of
                NotPrimitive -> do
                    reordered <- mapSet (finiteSet [True, False])
                    mapDeterministic (deterministicFromFunction reordered target not)
                BadPrimitive -> Left ExactPrimitiveFailure
        , interpretStochasticPrimitive = interpretStochasticPrimitive exactInterpreter
        }

emptyExactInterpreter :: ExactPrimitiveInterpreter EmptyPrimitive CostError
emptyExactInterpreter =
    ExactPrimitiveInterpreter
        { interpretDeterministicPrimitive = \_ _ primitive -> case primitive of {}
        , interpretStochasticPrimitive = \_ _ primitive -> case primitive of {}
        }

emptyCostInterpreter :: CircuitCostInterpreter EmptyPrimitive Owner CostError
emptyCostInterpreter =
    CircuitCostInterpreter
        { costDeterministicPrimitive = \_ _ primitive -> case primitive of {}
        , costStochasticPrimitive = \_ _ primitive -> case primitive of {}
        , costDeterministicTable = \_ -> Right (primitiveCharge TableOwner 3)
        }

largeLimits :: CircuitCostLimits
largeLimits = CircuitCostLimits 1000 1000 1000 1000000 10

rewriteLimits :: RewriteCheckLimits
rewriteLimits = RewriteCheckLimits 1000 1000 1000000

assertR :: String -> Bool -> IO ()
assertR label condition = if condition then pure () else ioError (userError label)

requireRightR :: (Show error) => String -> Either error value -> IO value
requireRightR _ (Right value) = pure value
requireRightR label (Left problem) = ioError (userError (label ++ ": " ++ show problem))

setR :: (Eq value, Show value) => [value] -> IO (FiniteSet value)
setR values = requireRightR "finite rewrite set" (finiteSet values)

nnR :: Rational -> NonNegativeRational
nnR value =
    case nonNegativeRational value of
        Right scalar -> scalar
        Left problem -> error (show problem)

testCostReports :: IO ()
testCostReports = do
    unit <- setR [()]
    boolean <- setR [False, True]
    reordered <- setR [True, False]
    empty <- setR ([] :: [Bool])
    let negation = deterministicPrimitive boolean boolean NotPrimitive
        coin = stochasticPrimitive unit boolean FairCoinPrimitive
    twice <- requireRightR "cost composition" (composeCircuit negation negation)
    let parallel = tensorCircuit negation negation
    composedReport <- requireRightR "composition cost" (interpretCircuitCost largeLimits costInterpreter twice)
    repeatedReport <- requireRightR "repeated composition cost" (interpretCircuitCost largeLimits costInterpreter twice)
    tensorReport <- requireRightR "tensor cost" (interpretCircuitCost largeLimits costInterpreter parallel)
    assertR "cost report was not deterministic" (composedReport == repeatedReport)
    assertR "composition did not add primitive work" (declaredPrimitiveWork composedReport == 14)
    assertR "composition primitive count changed" (deterministicPrimitiveCount composedReport == 2)
    assertR "composition structure was not charged" (compositionCount (structuralCounts composedReport) == 1)
    assertR "tensor did not add primitive work" (declaredPrimitiveWork tensorReport == 14)
    assertR "tensor structure was not charged" (tensorCount (structuralCounts tensorReport) == 1)
    assertR "owner total was not stable" (ownerCosts composedReport == [OwnerCost LogicOwner 14])

    weakenedReport <- requireRightR "weakening cost" (interpretCircuitCost largeLimits costInterpreter (weakenPurity negation))
    assertR "weakening changed primitive work" (declaredPrimitiveWork weakenedReport == 7)
    assertR "weakening was not counted" (weakeningCount (structuralCounts weakenedReport) == 1)

    mixture <-
        requireRightR
            "cost convex choice"
            (convexChoice ((nnR (1 % 2), weakenPurity negation) :| [(nnR (1 % 2), weakenPurity negation)]))
    mixtureReport <- requireRightR "convex cost" (interpretCircuitCost largeLimits costInterpreter mixture)
    assertR "convex cost did not charge every branch" (declaredPrimitiveWork mixtureReport == 14)
    assertR "convex structure was not charged" (convexChoiceCount (structuralCounts mixtureReport) == 1)

    zeroWeightBad <-
        requireRightR
            "zero-weight represented branch"
            (convexChoice ((nnR 1, weakenPurity negation) :| [(nnR 0, weakenPurity (deterministicPrimitive boolean boolean BadPrimitive))]))
    case interpretCircuitCost largeLimits costInterpreter zeroWeightBad of
        Left (CircuitCostPrimitiveError CostCallbackFailure) -> pure ()
        result -> ioError (userError ("zero-weight convex branch was not charged: " ++ show result))

    explicitShare <- requireRightR "explicit sharing" (composeCircuit coin (copyCircuit boolean))
    let shared = shareCircuit coin
    sharedReport <- requireRightR "shared cost" (interpretCircuitCost largeLimits costInterpreter shared)
    explicitShareReport <- requireRightR "explicit shared cost" (interpretCircuitCost largeLimits costInterpreter explicitShare)
    assertR "sharing elaboration cost changed" (withoutSyntax sharedReport == withoutSyntax explicitShareReport)
    assertR "sharing copy rows changed" (totalRepresentedCopyRows sharedReport == 2)

    fanout <- requireRightR "fanout" (fanoutCircuit coin coin)
    explicitFanout <- requireRightR "explicit fanout" (composeCircuit (copyCircuit unit) (tensorCircuit coin coin))
    fanoutReport <- requireRightR "fanout cost" (interpretCircuitCost largeLimits costInterpreter fanout)
    explicitFanoutReport <- requireRightR "explicit fanout cost" (interpretCircuitCost largeLimits costInterpreter explicitFanout)
    assertR "fanout elaboration cost changed" (withoutSyntax fanoutReport == withoutSyntax explicitFanoutReport)
    assertR "fanout input-copy rows changed" (totalRepresentedCopyRows fanoutReport == 1)

    emptyReportValue <- requireRightR "empty layout cost" (interpretCircuitCost largeLimits costInterpreter (identityCircuit empty))
    assertR "empty layout acquired cardinality" (maximumLiveLayoutCardinality emptyReportValue == 0)
    assertR "empty layout acquired matrix cells" (maximumRepresentedMatrixCells emptyReportValue == 0)
    orderedReport <- requireRightR "ordered layout cost" (interpretCircuitCost largeLimits costInterpreter (identityCircuit boolean))
    reorderedReport <- requireRightR "reordered layout cost" (interpretCircuitCost largeLimits costInterpreter (identityCircuit reordered))
    assertR "reordering changed represented cardinality cost" (withoutSyntax orderedReport == withoutSyntax reorderedReport)
    assertStructure "identity" identityCount (identityCircuit boolean)
    assertStructure "symmetry" symmetryCount (symmetryCircuit boolean boolean)
    assertStructure "associator" associatorCount (associateCircuit boolean boolean boolean)
    assertStructure "unassociator" unassociatorCount (unassociateCircuit boolean boolean boolean)
    assertStructure "left unitor" leftUnitorCount (leftUnitorCircuit boolean)
    assertStructure "left unitor inverse" leftUnitorInverseCount (leftUnitorInverseCircuit boolean)
    assertStructure "right unitor" rightUnitorCount (rightUnitorCircuit boolean)
    assertStructure "right unitor inverse" rightUnitorInverseCount (rightUnitorInverseCircuit boolean)
    assertStructure "copy" copyCount (copyCircuit boolean)
    assertStructure "discard" discardCount (discardCircuit boolean)
    assertR "deterministic rendering changed" (renderCircuitCostReport composedReport == renderCircuitCostReport repeatedReport)
  where
    withoutSyntax report = report{syntaxNodeCount = 0}

testConstructorCostDifferential :: IO ()
testConstructorCostDifferential = do
    unit <- setR [()]
    boolean <- setR [False, True]
    let negation = deterministicPrimitive boolean boolean NotPrimitive
        coin = stochasticPrimitive unit boolean FairCoinPrimitive
        assertNodes label expected circuit = do
            report <- requireRightR label (interpretCircuitCost largeLimits costInterpreter circuit)
            assertR (label ++ ": fold/report node differential") (syntaxNodeCount report == expected)
    table <- requireRightR "differential table" (deterministicTable boolean boolean [(False, False), (True, True)])
    composed <- requireRightR "differential composition" (composeCircuit negation negation)
    mixture <-
        requireRightR
            "differential convex choice"
            (convexChoice ((nnR (1 % 2), coin) :| [(nnR (1 % 2), coin)]))
    assertNodes "deterministic primitive" 1 negation
    assertNodes "stochastic primitive" 1 coin
    assertNodes "quoted deterministic table" 1 table
    assertNodes "identity" 1 (identityCircuit boolean)
    assertNodes "composition" 3 composed
    assertNodes "tensor" 3 (tensorCircuit negation negation)
    assertNodes "symmetry" 1 (symmetryCircuit boolean boolean)
    assertNodes "associator" 1 (associateCircuit boolean boolean boolean)
    assertNodes "unassociator" 1 (unassociateCircuit boolean boolean boolean)
    assertNodes "left unitor" 1 (leftUnitorCircuit boolean)
    assertNodes "left unitor inverse" 1 (leftUnitorInverseCircuit boolean)
    assertNodes "right unitor" 1 (rightUnitorCircuit boolean)
    assertNodes "right unitor inverse" 1 (rightUnitorInverseCircuit boolean)
    assertNodes "copy" 1 (copyCircuit boolean)
    assertNodes "discard" 1 (discardCircuit boolean)
    assertNodes "convex choice" 3 mixture
    assertNodes "weakening" 2 (weakenPurity negation)

testCostLimits :: IO ()
testCostLimits = do
    boolean <- setR [False, True]
    let negation = deterministicPrimitive boolean boolean NotPrimitive
        bad = deterministicPrimitive boolean boolean BadPrimitive
    compound <- requireRightR "budget compound" (composeCircuit negation bad)
    twice <- requireRightR "exact syntax compound" (composeCircuit negation negation)
    _ <- requireRightR "exact syntax boundary" (interpretCircuitCost largeLimits{maximumSyntaxNodes = 3} costInterpreter twice)
    case interpretCircuitCost largeLimits{maximumSyntaxNodes = 2} costInterpreter compound of
        Left (CircuitCostLimitExceeded CostSyntaxNodes 2 3) -> pure ()
        result -> ioError (userError ("node budget did not stop before right callback: " ++ show result))
    _ <- requireRightR "exact work boundary" (interpretCircuitCost largeLimits{maximumDeclaredWork = 7} costInterpreter negation)
    _ <- requireRightR "exact layout boundary" (interpretCircuitCost largeLimits{maximumLayoutCardinality = 2} costInterpreter negation)
    _ <- requireRightR "exact matrix boundary" (interpretCircuitCost largeLimits{maximumMatrixCells = 4} costInterpreter negation)
    expectLimit CostDeclaredWork 6 7 largeLimits{maximumDeclaredWork = 6} negation
    expectLimit CostLayoutCardinality 1 2 largeLimits{maximumLayoutCardinality = 1} negation
    expectLimit CostMatrixCells 3 4 largeLimits{maximumMatrixCells = 3} negation

    table <- requireRightR "owner table" (deterministicTable boolean boolean [(False, False), (True, True)])
    mixedOwners <- requireRightR "mixed-owner composition" (composeCircuit negation table)
    mixedOwnerReport <- requireRightR "mixed-owner report" (interpretCircuitCost largeLimits costInterpreter mixedOwners)
    assertR "owner first-occurrence order changed" (ownerCosts mixedOwnerReport == [OwnerCost LogicOwner 7, OwnerCost TableOwner 3])
    _ <- requireRightR "exact owner boundary" (interpretCircuitCost largeLimits{maximumOwnerEntries = 2} costInterpreter mixedOwners)
    case interpretCircuitCost largeLimits{maximumOwnerEntries = 1} costInterpreter mixedOwners of
        Left (CircuitCostLimitExceeded CostOwnerEntries 1 2) -> pure ()
        result -> ioError (userError ("owner-entry budget was not enforced: " ++ show result))
    case interpretCircuitCost largeLimits costInterpreter bad of
        Left (CircuitCostPrimitiveError CostCallbackFailure) -> pure ()
        result -> ioError (userError ("primitive callback error changed: " ++ show result))
  where
    expectLimit expectedDimension expectedLimit expectedActual limits circuit =
        case interpretCircuitCost limits costInterpreter circuit of
            Left (CircuitCostLimitExceeded actualDimension actualLimit actualValue)
                | actualDimension == expectedDimension
                    && actualLimit == expectedLimit
                    && actualValue == expectedActual ->
                    pure ()
            result -> ioError (userError ("cost limit was not enforced exactly: " ++ show result))

testRewriteCertificates :: IO ()
testRewriteCertificates = do
    boolean <- setR [False, True]
    reordered <- setR [True, False]
    empty <- setR ([] :: [Bool])
    let negation = deterministicPrimitive boolean boolean NotPrimitive
    leftCandidate <- requireRightR "left identity candidate" (leftIdentityRewrite negation)
    rightCandidate <- requireRightR "right identity candidate" (rightIdentityRewrite negation)
    reassociationCandidate <- requireRightR "reassociation candidate" (reassociateCompositionRewrite negation negation negation)
    leftChecked <- requireRightR "left identity check" (checkDeterministicRewrite rewriteLimits exactInterpreter leftCandidate)
    rightChecked <- requireRightR "right identity check" (checkDeterministicRewrite rewriteLimits exactInterpreter rightCandidate)
    reassociationChecked <- requireRightR "reassociation check" (checkDeterministicRewrite rewriteLimits exactInterpreter reassociationCandidate)
    dedupChecked <- requireRightR "deduplication check" (checkDeterministicRewrite rewriteLimits exactInterpreter (deduplicateDeterministicRewrite negation))
    assertR "left rule changed" (checkedRewriteRule leftChecked == RemoveLeftIdentity)
    assertR "right rule changed" (checkedRewriteRule rightChecked == RemoveRightIdentity)
    assertR "reassociation rule changed" (checkedRewriteRule reassociationChecked == ReassociateComposition)
    assertR "deduplication rule changed" (checkedRewriteRule dedupChecked == DeduplicateDeterministicFanout)

    emptyCandidate <- requireRightR "empty endpoint candidate" (leftIdentityRewrite (identityCircuit empty))
    _ <- requireRightR "empty endpoint certificate" (checkDeterministicRewrite rewriteLimits emptyExactInterpreter emptyCandidate)
    reorderedTable <-
        requireRightR
            "reordered endpoint table"
            (deterministicTable reordered reordered [(True, False), (False, True)])
    reorderedCandidate <- requireRightR "reordered endpoint candidate" (rightIdentityRewrite reorderedTable)
    _ <- requireRightR "reordered endpoint certificate" (checkDeterministicRewrite rewriteLimits emptyExactInterpreter reorderedCandidate)

    leftCosts <- requireRightR "left identity costs" (compareCheckedRewriteCosts largeLimits costInterpreter leftChecked)
    assertR "identity removal changed primitive work" (declaredWorkReduction leftCosts == 0)
    assertR
        "identity removal did not remove structural identity and composition"
        ( identityCount (structuralCounts (originalRewriteCost leftCosts))
            - identityCount (structuralCounts (resultRewriteCost leftCosts))
            == 1
            && compositionCount (structuralCounts (originalRewriteCost leftCosts))
                - compositionCount (structuralCounts (resultRewriteCost leftCosts))
                == 1
        )
    reassociationCosts <- requireRightR "reassociation costs" (compareCheckedRewriteCosts largeLimits costInterpreter reassociationChecked)
    assertR "reassociation changed declared cost" (originalRewriteCost reassociationCosts == resultRewriteCost reassociationCosts)
    dedupCosts <- requireRightR "deduplication costs" (compareCheckedRewriteCosts largeLimits costInterpreter dedupChecked)
    repeatedDedupCosts <- requireRightR "repeated deduplication costs" (compareCheckedRewriteCosts largeLimits costInterpreter dedupChecked)
    assertR "deduplication did not remove one charged leaf occurrence" (chargedLeafOccurrenceReduction dedupCosts == 1)
    assertR "deduplication primitive work reduction changed" (declaredWorkReduction dedupCosts == 7)
    assertR "rewrite report was not deterministic" (renderRewriteCostReport dedupCosts == renderRewriteCostReport repeatedDedupCosts)
    assertR "semantic rewrite report acquired timing data" (not ("time" `isInfixOf` renderRewriteCostReport dedupCosts))

    zeroCosts <- requireRightR "zero declared rewrite cost" (compareCheckedRewriteCosts largeLimits zeroCostInterpreter dedupChecked)
    assertR "zero declared cost fabricated an improvement" (declaredWorkReduction zeroCosts == 0)

    mapM_ (checkBooleanFunction boolean) [const False, id, not, const True]

    quoted <- requireRightR "compiled quoted not" (quotedTableTerm boolean boolean [(False, True), (True, False)])
    compiled <- requireRightR "compiled quoted circuit" (compileDeterministicTerm quoted)
    compiledChecked <- requireRightR "compiled deduplication" (checkDeterministicRewrite rewriteLimits emptyExactInterpreter (deduplicateDeterministicRewrite compiled))
    compiledCosts <- requireRightR "compiled deduplication costs" (compareCheckedRewriteCosts largeLimits emptyCostInterpreter compiledChecked)
    assertR "compiled deterministic term did not deduplicate" (chargedLeafOccurrenceReduction compiledCosts == 1)

    originalMatrix <- requireRightR "checked original matrix" (interpretDeterministicCircuit exactInterpreter (checkedRewriteOriginal dedupChecked))
    resultMatrix <- requireRightR "checked result matrix" (interpretDeterministicCircuit exactInterpreter (checkedRewriteResult dedupChecked))
    assertR "checked endpoint source layout changed" (sameFiniteSetLayout (deterministicSource originalMatrix) (deterministicSource resultMatrix))
    assertR "checked endpoint target layout changed" (sameFiniteSetLayout (deterministicTarget originalMatrix) (deterministicTarget resultMatrix))
    assertR "checked row-major matrix layout changed" (sameMatrixLayout (forgetDeterministic originalMatrix) (forgetDeterministic resultMatrix))

testRewriteCounterexamples :: IO ()
testRewriteCounterexamples = do
    unit <- setR [()]
    boolean <- setR [False, True]
    let coin = stochasticPrimitive unit boolean FairCoinPrimitive
        shared = shareCircuit coin
        dirac = stochasticPrimitive boolean boolean DiracStochasticPrimitive
    independent <- requireRightR "independent fair coins" (fanoutCircuit coin coin)
    sharedMatrix <- requireRightR "shared fair coin" (interpretExactCircuit exactInterpreter shared)
    independentMatrix <- requireRightR "independent fair coins" (interpretExactCircuit exactInterpreter independent)
    assertR "shared fair coin acquired off-diagonal mass" (entry sharedMatrix () (False, True) == 0)
    assertR "independent fair coins lost off-diagonal mass" (entry independentMatrix () (False, True) == 1 % 4)
    assertR "shared fair coin equalled two executions" (not (stochasticEquivalent sharedMatrix independentMatrix))
    case circuitPurity dirac of
        SStochastic -> pure ()

    let negation = deterministicPrimitive boolean boolean NotPrimitive
    candidate <- requireRightR "budgeted identity candidate" (leftIdentityRewrite negation)
    case checkDeterministicRewrite rewriteLimits{rewriteMaximumSyntaxNodes = 1} exactInterpreter candidate of
        Left (RewriteAnalysisError RewriteOriginalSide (CircuitCostLimitExceeded CostSyntaxNodes 1 2)) -> pure ()
        result -> ioError (userError ("rewrite checker ignored syntax budget: " ++ showChecked result))
    case checkDeterministicRewrite rewriteLimits reorderedExactInterpreter candidate of
        Left (RewriteInterpretationError RewriteOriginalSide (CircuitFoldAlgebraError ExactCircuitPrimitiveSourceMismatch)) -> pure ()
        result -> ioError (userError ("rewrite checker accepted reordered primitive endpoint: " ++ showChecked result))

    primitiveFailureCandidate <-
        requireRightR
            "primitive failure candidate"
            (leftIdentityRewrite (deterministicPrimitive boolean boolean BadPrimitive))
    case checkDeterministicRewrite rewriteLimits exactInterpreter primitiveFailureCandidate of
        Left
            ( RewriteInterpretationError
                    RewriteOriginalSide
                    (CircuitFoldAlgebraError (ExactCircuitPrimitiveError ExactPrimitiveFailure))
                ) -> pure ()
        result -> ioError (userError ("rewrite checker changed the primitive error payload: " ++ showChecked result))
  where
    showChecked (Left problem) = show problem
    showChecked (Right checked) = show (checkedRewriteRule checked)

assertStructure ::
    String ->
    (CircuitStructuralCounts -> Natural) ->
    Circuit CostPrimitive purity source target ->
    IO ()
assertStructure label count circuit = do
    report <- requireRightR (label ++ " cost") (interpretCircuitCost largeLimits costInterpreter circuit)
    assertR (label ++ " was not counted") (count (structuralCounts report) == 1)

checkBooleanFunction :: FiniteSet Bool -> (Bool -> Bool) -> IO ()
checkBooleanFunction boolean function = do
    table <-
        requireRightR
            "Boolean total-function table"
            (deterministicTable boolean boolean [(False, function False), (True, function True)])
    checked <- requireRightR "Boolean copy certificate" (checkDeterministicRewrite rewriteLimits exactInterpreter (deduplicateDeterministicRewrite table))
    original <- requireRightR "Boolean original" (interpretDeterministicCircuit exactInterpreter (checkedRewriteOriginal checked))
    result <- requireRightR "Boolean result" (interpretDeterministicCircuit exactInterpreter (checkedRewriteResult checked))
    assertR "Boolean total function failed deterministic copy" (deterministicEquivalent original result)

entry :: StochasticMatrix NonNegativeRational source target -> source -> target -> Rational
entry arrow source target = getNonNegativeRational (fromMaybe zero (matrixEntry (forgetStochastic arrow) source target))

fairCoin ::
    FiniteSet () ->
    FiniteSet Bool ->
    Either CostError (StochasticMatrix NonNegativeRational () Bool)
fairCoin source target = do
    raw <-
        case matrixFromRows source target [[nnR (1 % 2), nnR (1 % 2)]] of
            Left _ -> Left ExactPrimitiveFailure
            Right matrix -> Right matrix
    case stochasticMatrix raw of
        Left _ -> Left ExactPrimitiveFailure
        Right matrix -> Right matrix

mapDeterministic :: Either error value -> Either CostError value
mapDeterministic = either (const (Left ExactPrimitiveFailure)) Right

mapSet :: Either error value -> Either CostError value
mapSet = either (const (Left ExactPrimitiveFailure)) Right
