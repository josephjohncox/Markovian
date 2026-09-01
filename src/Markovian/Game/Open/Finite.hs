{-# LANGUAGE RoleAnnotations #-}

{- | Owner-refined finite open games with bounded pure equilibrium checks.

This module is separate from arena histories.  An open game has finite strategy
profiles, play, coplay, and a context-indexed best-response relation.  It has no
chance, mixed strategies, repeated play, Bayesian information, or
subgame-perfect solver.
-}
module Markovian.Game.Open.Finite (
    StrategySchema,
    unitStrategySchema,
    ownedStrategySchema,
    strategySchemaOwners,
    strategySchemaProfiles,
    StrategyBijection,
    unitStrategyBijection,
    ownedStrategyBijection,
    productStrategyBijection,
    leftUnitStrategyBijection,
    rightUnitStrategyBijection,
    associatorStrategyBijection,
    symmetryStrategyBijection,
    FiniteOpenGame,
    FiniteOpenGameError (..),
    finiteOpenGame,
    openGameStrategySchema,
    openGameSource,
    openGameCoutility,
    openGameTarget,
    openGameUtility,
    playOpenGame,
    coplayOpenGame,
    OpenGameContext,
    OpenGameContextError (..),
    openGameContext,
    contextInput,
    contextContinuation,
    OpenGameQueryError (..),
    bestResponse,
    StrategyOpticError (..),
    strategyOptic,
    OpenGameCompositionError (..),
    identityOpenGame,
    composeOpenGame,
    tensorOpenGame,
    EquilibriumBudget,
    equilibriumBudget,
    EquilibriumError (..),
    EquilibriumReport (..),
    enumeratePureEquilibria,
    enumerateBestResponses,
    ObservationBudget,
    observationBudget,
    ObservationError (..),
    ObservationReport (..),
    observationallyEqualUnder,
    sameOpenGameLayout,
) where

import Data.List (find, foldl')
import Markovian.Category.Finite.Set (FiniteSet, finiteSet, finiteSetCardinality, finiteSetValues, sameFiniteSet)
import Markovian.Category.Finite.Set qualified as FiniteSet
import Markovian.Game.Optic.Finite
import Numeric.Natural (Natural)

{- | Structural decision-site ownership. Products retain their association, so
a whole-profile permutation cannot silently exchange owner-controlled leaves.
-}
data SchemaShape owner
    = UnitSchemaShape
    | OwnedSchemaShape !owner
    | ProductSchemaShape !(SchemaShape owner) !(SchemaShape owner)
    deriving (Eq, Show)

-- | Opaque strategy-profile layout with a structural ownership tree.
data StrategySchema owner strategy = StrategySchema !(SchemaShape owner) !(FiniteSet strategy)

type role StrategySchema nominal nominal

-- | The owner-free singleton strategy schema.
unitStrategySchema :: StrategySchema owner ()
unitStrategySchema = StrategySchema UnitSchemaShape singletonUnit

-- | One owner controlling one represented local strategy set.
ownedStrategySchema :: (Eq owner) => owner -> FiniteSet strategy -> StrategySchema owner strategy
ownedStrategySchema owner = StrategySchema (OwnedSchemaShape owner)

-- | Read owners in structural left-to-right decision-site order.
strategySchemaOwners :: StrategySchema owner strategy -> [owner]
strategySchemaOwners (StrategySchema shape _) = shapeOwners shape
  where
    shapeOwners UnitSchemaShape = []
    shapeOwners (OwnedSchemaShape owner) = [owner]
    shapeOwners (ProductSchemaShape left right) = shapeOwners left ++ shapeOwners right

-- | Read profiles in represented strategy order.
strategySchemaProfiles :: StrategySchema owner strategy -> FiniteSet strategy
strategySchemaProfiles (StrategySchema _ profiles) = profiles

schemaShape :: StrategySchema owner strategy -> SchemaShape owner
schemaShape (StrategySchema shape _) = shape

{- | A structurally owner-preserving strategy bijection. Its constructor is
private. Leaf bijections may relayout one owner's local carrier; products,
units, association, and symmetry are represented explicitly.
-}
data StrategyBijection owner left right
    = StrategyBijection
        !(SchemaShape owner)
        !(SchemaShape owner)
        !(FiniteBijection left right)

type role StrategyBijection nominal nominal nominal

-- | Structural identity on the owner-free unit carrier.
unitStrategyBijection :: FiniteBudget -> Either (FiniteBijectionError () ()) (StrategyBijection owner () ())
unitStrategyBijection budget = StrategyBijection UnitSchemaShape UnitSchemaShape <$> identityFiniteBijection budget singletonUnit

-- | Relayout one local carrier without changing its owner leaf.
ownedStrategyBijection :: owner -> FiniteBijection left right -> StrategyBijection owner left right
ownedStrategyBijection owner = StrategyBijection (OwnedSchemaShape owner) (OwnedSchemaShape owner)

-- | Combine two structural leaf-preserving witnesses.
productStrategyBijection ::
    (Eq leftA, Eq rightA, Eq leftB, Eq rightB) =>
    FiniteBudget ->
    StrategyBijection owner leftA rightA ->
    StrategyBijection owner leftB rightB ->
    Either (FiniteBijectionError (leftA, leftB) (rightA, rightB)) (StrategyBijection owner (leftA, leftB) (rightA, rightB))
productStrategyBijection budget (StrategyBijection leftShapeA rightShapeA first) (StrategyBijection leftShapeB rightShapeB second) = do
    left <- productSet (finiteBijectionLeft first) (finiteBijectionLeft second)
    right <- productSet (finiteBijectionRight first) (finiteBijectionRight second)
    bijection <- finiteBijection budget left right [((a, b), (required "first leaf bijection" (applyFiniteBijection first a), required "second leaf bijection" (applyFiniteBijection second b))) | (a, b) <- finiteSetValues left]
    Right (StrategyBijection (ProductSchemaShape leftShapeA leftShapeB) (ProductSchemaShape rightShapeA rightShapeB) bijection)

-- | Remove a structural unit on the left.
leftUnitStrategyBijection ::
    (Eq strategy) =>
    FiniteBudget ->
    StrategySchema owner strategy ->
    Either (FiniteBijectionError ((), strategy) strategy) (StrategyBijection owner ((), strategy) strategy)
leftUnitStrategyBijection budget schema = do
    left <- productSet singletonUnit (strategySchemaProfiles schema)
    bijection <- finiteBijection budget left (strategySchemaProfiles schema) [(((), strategy), strategy) | strategy <- finiteSetValues (strategySchemaProfiles schema)]
    Right (StrategyBijection (ProductSchemaShape UnitSchemaShape (schemaShape schema)) (schemaShape schema) bijection)

-- | Remove a structural unit on the right.
rightUnitStrategyBijection ::
    (Eq strategy) =>
    FiniteBudget ->
    StrategySchema owner strategy ->
    Either (FiniteBijectionError (strategy, ()) strategy) (StrategyBijection owner (strategy, ()) strategy)
rightUnitStrategyBijection budget schema = do
    left <- productSet (strategySchemaProfiles schema) singletonUnit
    bijection <- finiteBijection budget left (strategySchemaProfiles schema) [((strategy, ()), strategy) | strategy <- finiteSetValues (strategySchemaProfiles schema)]
    Right (StrategyBijection (ProductSchemaShape (schemaShape schema) UnitSchemaShape) (schemaShape schema) bijection)

-- | Reassociate three ownership/profile trees without exchanging leaves.
associatorStrategyBijection ::
    (Eq a, Eq b, Eq c) =>
    FiniteBudget ->
    StrategySchema owner a ->
    StrategySchema owner b ->
    StrategySchema owner c ->
    Either (FiniteBijectionError ((a, b), c) (a, (b, c))) (StrategyBijection owner ((a, b), c) (a, (b, c)))
associatorStrategyBijection budget aSchema bSchema cSchema = do
    ab <- productSet (strategySchemaProfiles aSchema) (strategySchemaProfiles bSchema)
    left <- productSet ab (strategySchemaProfiles cSchema)
    bc <- productSet (strategySchemaProfiles bSchema) (strategySchemaProfiles cSchema)
    right <- productSet (strategySchemaProfiles aSchema) bc
    bijection <- finiteBijection budget left right [(((a, b), c), (a, (b, c))) | ((a, b), c) <- finiteSetValues left]
    Right (StrategyBijection (ProductSchemaShape (ProductSchemaShape (schemaShape aSchema) (schemaShape bSchema)) (schemaShape cSchema)) (ProductSchemaShape (schemaShape aSchema) (ProductSchemaShape (schemaShape bSchema) (schemaShape cSchema))) bijection)

-- | Exchange two complete subtrees, carrying each owner with its local carrier.
symmetryStrategyBijection ::
    (Eq a, Eq b) =>
    FiniteBudget ->
    StrategySchema owner a ->
    StrategySchema owner b ->
    Either (FiniteBijectionError (a, b) (b, a)) (StrategyBijection owner (a, b) (b, a))
symmetryStrategyBijection budget aSchema bSchema = do
    left <- productSet (strategySchemaProfiles aSchema) (strategySchemaProfiles bSchema)
    right <- productSet (strategySchemaProfiles bSchema) (strategySchemaProfiles aSchema)
    bijection <- finiteBijection budget left right [((a, b), (b, a)) | (a, b) <- finiteSetValues left]
    Right (StrategyBijection (ProductSchemaShape (schemaShape aSchema) (schemaShape bSchema)) (ProductSchemaShape (schemaShape bSchema) (schemaShape aSchema)) bijection)

{- | A checked finite open game.  The best-response callback owns its internal
termination and resource use; all framework enumeration around it is bounded.
-}
data FiniteOpenGame owner strategy x s y r
    = FiniteOpenGame
        !(StrategySchema owner strategy)
        !(FiniteSet x)
        !(FiniteSet s)
        !(FiniteSet y)
        !(FiniteSet r)
        !(FiniteFunction (strategy, x) y)
        !(FiniteFunction (strategy, x, r) s)
        !(x -> FiniteFunction y r -> strategy -> strategy -> Bool)

type role FiniteOpenGame nominal nominal nominal nominal nominal nominal

-- | Open-game table construction failures.
data FiniteOpenGameError strategy x s y r
    = OpenGameWorkLimitExceeded !Natural !Natural
    | OpenGamePlayTableError !(FiniteFunctionError (strategy, x) y)
    | OpenGameCoplayTableError !(FiniteFunctionError (strategy, x, r) s)
    deriving (Eq, Show)

{- | Construct a game from complete play and coplay tables and a best-response
membership evaluator.  Tables are canonicalized to strategy-major order.
-}
finiteOpenGame ::
    (Eq strategy, Eq x, Eq s, Eq y, Eq r) =>
    FiniteBudget ->
    StrategySchema owner strategy ->
    FiniteSet x ->
    FiniteSet s ->
    FiniteSet y ->
    FiniteSet r ->
    [((strategy, x), y)] ->
    [((strategy, x, r), s)] ->
    (x -> FiniteFunction y r -> strategy -> strategy -> Bool) ->
    Either (FiniteOpenGameError strategy x s y r) (FiniteOpenGame owner strategy x s y r)
finiteOpenGame budget schema x s y r playEntries coplayEntries responseEvaluator = do
    let profiles = strategySchemaProfiles schema
        playWork = cardinality profiles * cardinality x
        coplayWork = playWork * cardinality r
        requiredWork = playWork + coplayWork
    if requiredWork > maximumFiniteWork budget
        then Left (OpenGameWorkLimitExceeded requiredWork (maximumFiniteWork budget))
        else do
            strategyInputs <- productSet profiles x
            strategyInputUtilities <- tripleSet profiles x r
            play <- mapLeft OpenGamePlayTableError (finiteFunction budget strategyInputs y playEntries)
            coplay <- mapLeft OpenGameCoplayTableError (finiteFunction budget strategyInputUtilities s coplayEntries)
            Right (FiniteOpenGame schema x s y r play coplay responseEvaluator)

-- | Read the checked owner and profile schema.
openGameStrategySchema :: FiniteOpenGame owner strategy x s y r -> StrategySchema owner strategy
openGameStrategySchema (FiniteOpenGame schema _ _ _ _ _ _ _) = schema

-- | Read the forward source carrier.
openGameSource :: FiniteOpenGame owner strategy x s y r -> FiniteSet x
openGameSource (FiniteOpenGame _ x _ _ _ _ _ _) = x

-- | Read the backward result carrier.
openGameCoutility :: FiniteOpenGame owner strategy x s y r -> FiniteSet s
openGameCoutility (FiniteOpenGame _ _ s _ _ _ _ _) = s

-- | Read the forward target carrier.
openGameTarget :: FiniteOpenGame owner strategy x s y r -> FiniteSet y
openGameTarget (FiniteOpenGame _ _ _ y _ _ _ _) = y

-- | Read the incoming utility carrier.
openGameUtility :: FiniteOpenGame owner strategy x s y r -> FiniteSet r
openGameUtility (FiniteOpenGame _ _ _ _ r _ _ _) = r

-- | Query checked play.  Outside profiles or inputs return 'Nothing'.
playOpenGame :: (Eq strategy, Eq x) => FiniteOpenGame owner strategy x s y r -> strategy -> x -> Maybe y
playOpenGame (FiniteOpenGame _ _ _ _ _ play _ _) strategy input = applyFiniteFunction play (strategy, input)

-- | Query checked coplay.  Outside profiles, inputs, or utilities return 'Nothing'.
coplayOpenGame :: (Eq strategy, Eq x, Eq r) => FiniteOpenGame owner strategy x s y r -> strategy -> x -> r -> Maybe s
coplayOpenGame (FiniteOpenGame _ _ _ _ _ _ coplay _) strategy input utility = applyFiniteFunction coplay (strategy, input, utility)

-- | A checked represented context @(x, k : Y -> R)@.
data OpenGameContext x y r = OpenGameContext !x !(FiniteFunction y r)

type role OpenGameContext nominal nominal nominal

-- | Context boundary failures.
data OpenGameContextError x
    = ContextInputOutsideSource !x
    | ContextContinuationSourceMismatch
    | ContextContinuationTargetMismatch
    deriving (Eq, Show)

-- | Check an input and continuation against labelled game boundaries.
openGameContext ::
    (Eq x, Eq y, Eq r) =>
    FiniteOpenGame owner strategy x s y r ->
    x ->
    FiniteFunction y r ->
    Either (OpenGameContextError x) (OpenGameContext x y r)
openGameContext game input continuation
    | input `notElem` finiteSetValues (openGameSource game) = Left (ContextInputOutsideSource input)
    | not (sameFiniteSet (finiteFunctionSource continuation) (openGameTarget game)) = Left ContextContinuationSourceMismatch
    | not (sameFiniteSet (finiteFunctionTarget continuation) (openGameUtility game)) = Left ContextContinuationTargetMismatch
    | otherwise =
        case finiteFunction
            (finiteBudget (cardinality (openGameTarget game)))
            (openGameTarget game)
            (openGameUtility game)
            [(output, applyRequired continuation output) | output <- finiteSetValues (openGameTarget game)] of
            Left _ -> error "openGameContext: checked continuation could not be canonicalized"
            Right canonical -> Right (OpenGameContext input canonical)

-- | Read the represented context input.
contextInput :: OpenGameContext x y r -> x
contextInput (OpenGameContext input _) = input

-- | Read the checked finite continuation.
contextContinuation :: OpenGameContext x y r -> FiniteFunction y r
contextContinuation (OpenGameContext _ continuation) = continuation

-- | Best-response query failures.
data OpenGameQueryError strategy
    = IncumbentStrategyOutsideSchema !strategy
    | DeviatingStrategyOutsideSchema !strategy
    | QueryContextBoundaryMismatch
    deriving (Eq, Show)

-- | Test membership @incumbent B deviation@ in one checked context.
bestResponse ::
    (Eq strategy, Eq x, Eq y, Eq r) =>
    FiniteOpenGame owner strategy x s y r ->
    OpenGameContext x y r ->
    strategy ->
    strategy ->
    Either (OpenGameQueryError strategy) Bool
bestResponse game context incumbent deviation
    | incumbent `notElem` profiles = Left (IncumbentStrategyOutsideSchema incumbent)
    | deviation `notElem` profiles = Left (DeviatingStrategyOutsideSchema deviation)
    | contextInput context `notElem` finiteSetValues (openGameSource game) = Left QueryContextBoundaryMismatch
    | not (sameFiniteSet (finiteFunctionSource (contextContinuation context)) (openGameTarget game)) = Left QueryContextBoundaryMismatch
    | not (sameFiniteSet (finiteFunctionTarget (contextContinuation context)) (openGameUtility game)) = Left QueryContextBoundaryMismatch
    | otherwise = Right (bestResponseEvaluator game (contextInput context) (contextContinuation context) incumbent deviation)
  where
    profiles = finiteSetValues (strategySchemaProfiles (openGameStrategySchema game))

-- | Failure to select a represented strategy or build its concrete optic.
data StrategyOpticError strategy x s y r
    = StrategyOpticUnknownStrategy !strategy
    | StrategyOpticConstructionError !(FiniteOpticError x s y r)
    deriving (Eq, Show)

-- | Extract the concrete optic for one represented strategy.
strategyOptic ::
    (Eq strategy, Eq x, Eq s, Eq y, Eq r) =>
    FiniteBudget ->
    FiniteOpenGame owner strategy x s y r ->
    strategy ->
    Either (StrategyOpticError strategy x s y r) (FiniteOptic x s y r)
strategyOptic budget game strategy
    | strategy `notElem` finiteSetValues (strategySchemaProfiles (openGameStrategySchema game)) = Left (StrategyOpticUnknownStrategy strategy)
    | otherwise =
        mapLeft StrategyOpticConstructionError $
            finiteOptic
                budget
                (openGameSource game)
                (openGameCoutility game)
                (openGameTarget game)
                (openGameUtility game)
                [(input, required "strategy optic play" (playOpenGame game strategy input)) | input <- finiteSetValues (openGameSource game)]
                [ ((input, utility), required "strategy optic coplay" (coplayOpenGame game strategy input utility))
                | input <- finiteSetValues (openGameSource game)
                , utility <- finiteSetValues (openGameUtility game)
                ]

-- | Sequential/tensor construction failures.
data OpenGameCompositionError owner
    = OpenGameForwardBoundaryMismatch
    | OpenGameBackwardBoundaryMismatch
    | OverlappingStrategyOwner !owner
    | OpenGameCompositionWorkLimitExceeded !Natural !Natural
    | OpenGameInternalConstructionFailure
    deriving (Eq, Show)

-- | The owner-free identity game.
identityOpenGame ::
    (Eq x, Eq s) =>
    FiniteBudget ->
    FiniteSet x ->
    FiniteSet s ->
    Either (FiniteOpenGameError () x s x s) (FiniteOpenGame owner () x s x s)
identityOpenGame budget x s =
    finiteOpenGame
        budget
        unitStrategySchema
        x
        s
        x
        s
        [(((), input), input) | input <- finiteSetValues x]
        [(((), input, utility), utility) | input <- finiteSetValues x, utility <- finiteSetValues s]
        (\_ _ _ _ -> True)

{- | Sequential composition using the incumbent downstream strategy in the
continuation sent to the upstream best-response relation.
-}
composeOpenGame ::
    (Eq owner, Eq firstStrategy, Eq secondStrategy, Eq x, Eq s, Eq y, Eq r, Eq z, Eq q) =>
    FiniteBudget ->
    FiniteOpenGame owner firstStrategy x s y r ->
    FiniteOpenGame owner secondStrategy y r z q ->
    Either (OpenGameCompositionError owner) (FiniteOpenGame owner (firstStrategy, secondStrategy) x s z q)
composeOpenGame budget first second
    | not (sameFiniteSet (openGameTarget first) (openGameSource second)) = Left OpenGameForwardBoundaryMismatch
    | not (sameFiniteSet (openGameUtility first) (openGameCoutility second)) = Left OpenGameBackwardBoundaryMismatch
    | Just owner <- firstOverlap first second = Left (OverlappingStrategyOwner owner)
    | requiredWork > maximumFiniteWork budget = Left (OpenGameCompositionWorkLimitExceeded requiredWork (maximumFiniteWork budget))
    | otherwise = do
        schema <- combineSchemas (openGameStrategySchema first) (openGameStrategySchema second)
        let profiles = finiteSetValues (strategySchemaProfiles schema)
            playEntries =
                [ ((profile, input), playSecond profile input)
                | profile <- profiles
                , input <- finiteSetValues x
                ]
            coplayEntries =
                [ ((profile, input, utility), coplayFirst profile input utility)
                | profile <- profiles
                , input <- finiteSetValues x
                , utility <- finiteSetValues q
                ]
        mapConstructionError budget $
            finiteOpenGame budget schema x s z q playEntries coplayEntries response
  where
    x = openGameSource first
    s = openGameCoutility first
    z = openGameTarget second
    q = openGameUtility second
    profileCount = cardinality (strategySchemaProfiles (openGameStrategySchema first)) * cardinality (strategySchemaProfiles (openGameStrategySchema second))
    playCount = profileCount * cardinality x
    coplayCount = playCount * cardinality q
    requiredWork = profileCount + playCount + coplayCount
    playSecond (firstStrategy, secondStrategy) input =
        required "composed second play" (playOpenGame second secondStrategy (required "composed first play" (playOpenGame first firstStrategy input)))
    coplayFirst (firstStrategy, secondStrategy) input utility =
        required
            "composed first coplay"
            ( coplayOpenGame
                first
                firstStrategy
                input
                ( required
                    "composed second coplay"
                    (coplayOpenGame second secondStrategy (required "composed middle play" (playOpenGame first firstStrategy input)) utility)
                )
            )
    response input continuation incumbent deviation =
        upstreamResponse && downstreamResponse
      where
        (incumbentFirst, incumbentSecond) = incumbent
        (deviationFirst, deviationSecond) = deviation
        upstreamContinuation =
            requiredEither
                "upstream continuation"
                ( finiteFunctionFromFunction
                    budget
                    (openGameTarget first)
                    (openGameUtility first)
                    (\middle -> required "upstream coplay" (coplayOpenGame second incumbentSecond middle (applyRequired continuation (required "upstream downstream play" (playOpenGame second incumbentSecond middle)))))
                )
        upstreamContext = OpenGameContext input upstreamContinuation
        downstreamInput = required "downstream input" (playOpenGame first incumbentFirst input)
        downstreamContext = OpenGameContext downstreamInput continuation
        upstreamResponse = requiredEither "upstream response" (bestResponse first upstreamContext incumbentFirst deviationFirst)
        downstreamResponse = requiredEither "downstream response" (bestResponse second downstreamContext incumbentSecond deviationSecond)

-- | Tensor composition with the other incumbent play held fixed in each induced continuation.
tensorOpenGame ::
    (Eq owner, Eq firstStrategy, Eq secondStrategy, Eq x1, Eq s1, Eq y1, Eq r1, Eq x2, Eq s2, Eq y2, Eq r2) =>
    FiniteBudget ->
    FiniteOpenGame owner firstStrategy x1 s1 y1 r1 ->
    FiniteOpenGame owner secondStrategy x2 s2 y2 r2 ->
    Either
        (OpenGameCompositionError owner)
        (FiniteOpenGame owner (firstStrategy, secondStrategy) (x1, x2) (s1, s2) (y1, y2) (r1, r2))
tensorOpenGame budget first second
    | Just owner <- firstOverlap first second = Left (OverlappingStrategyOwner owner)
    | requiredWork > maximumFiniteWork budget = Left (OpenGameCompositionWorkLimitExceeded requiredWork (maximumFiniteWork budget))
    | otherwise = do
        schema <- combineSchemas (openGameStrategySchema first) (openGameStrategySchema second)
        x <- internalProduct (openGameSource first) (openGameSource second)
        s <- internalProduct (openGameCoutility first) (openGameCoutility second)
        y <- internalProduct (openGameTarget first) (openGameTarget second)
        r <- internalProduct (openGameUtility first) (openGameUtility second)
        mapConstructionError budget $
            finiteOpenGame
                budget
                schema
                x
                s
                y
                r
                [ ((profile, input), tensorPlay profile input)
                | profile <- finiteSetValues (strategySchemaProfiles schema)
                , input <- finiteSetValues x
                ]
                [ ((profile, input, utility), tensorCoplay profile input utility)
                | profile <- finiteSetValues (strategySchemaProfiles schema)
                , input <- finiteSetValues x
                , utility <- finiteSetValues r
                ]
                response
  where
    profileCount = cardinality (strategySchemaProfiles (openGameStrategySchema first)) * cardinality (strategySchemaProfiles (openGameStrategySchema second))
    xCount = cardinality (openGameSource first) * cardinality (openGameSource second)
    sCount = cardinality (openGameCoutility first) * cardinality (openGameCoutility second)
    yCount = cardinality (openGameTarget first) * cardinality (openGameTarget second)
    rCount = cardinality (openGameUtility first) * cardinality (openGameUtility second)
    playCount = profileCount * xCount
    coplayCount = playCount * rCount
    requiredWork = profileCount + xCount + sCount + yCount + rCount + playCount + coplayCount
    tensorPlay (firstStrategy, secondStrategy) (firstInput, secondInput) =
        ( required "tensor first play" (playOpenGame first firstStrategy firstInput)
        , required "tensor second play" (playOpenGame second secondStrategy secondInput)
        )
    tensorCoplay (firstStrategy, secondStrategy) (firstInput, secondInput) (firstUtility, secondUtility) =
        ( required "tensor first coplay" (coplayOpenGame first firstStrategy firstInput firstUtility)
        , required "tensor second coplay" (coplayOpenGame second secondStrategy secondInput secondUtility)
        )
    response (firstInput, secondInput) continuation incumbent deviation = firstResponse && secondResponse
      where
        (incumbentFirst, incumbentSecond) = incumbent
        (deviationFirst, deviationSecond) = deviation
        fixedFirstPlay = required "fixed first play" (playOpenGame first incumbentFirst firstInput)
        fixedSecondPlay = required "fixed second play" (playOpenGame second incumbentSecond secondInput)
        firstContinuation =
            requiredEither
                "first tensor continuation"
                ( finiteFunctionFromFunction
                    budget
                    (openGameTarget first)
                    (openGameUtility first)
                    (\firstOutput -> fst (applyRequired continuation (firstOutput, fixedSecondPlay)))
                )
        secondContinuation =
            requiredEither
                "second tensor continuation"
                ( finiteFunctionFromFunction
                    budget
                    (openGameTarget second)
                    (openGameUtility second)
                    (\secondOutput -> snd (applyRequired continuation (fixedFirstPlay, secondOutput)))
                )
        firstContext = OpenGameContext firstInput firstContinuation
        secondContext = OpenGameContext secondInput secondContinuation
        firstResponse = requiredEither "first tensor response" (bestResponse first firstContext incumbentFirst deviationFirst)
        secondResponse = requiredEither "second tensor response" (bestResponse second secondContext incumbentSecond deviationSecond)

-- | Independent profile and relation-check limits.
data EquilibriumBudget = EquilibriumBudget !Natural !Natural
    deriving (Eq, Show)

-- | Set profile and relation-membership limits.
equilibriumBudget :: Natural -> Natural -> EquilibriumBudget
equilibriumBudget = EquilibriumBudget

-- | Equilibrium preflight failures.  No partial relation is returned.
data EquilibriumError
    = EquilibriumContextMismatch
    | EquilibriumProfileLimitExceeded !Natural !Natural
    | EquilibriumRelationLimitExceeded !Natural !Natural
    deriving (Eq, Show)

-- | Deterministic semantic evidence.  It contains no timing fields.
data EquilibriumReport strategy = EquilibriumReport
    { representedProfileCount :: !Natural
    , performedRelationChecks :: !Natural
    , configuredProfileLimit :: !Natural
    , configuredRelationLimit :: !Natural
    , equilibriumCompleted :: !Bool
    , equilibriumProfiles :: ![strategy]
    }
    deriving (Eq, Show)

-- | Enumerate contextual pure equilibria, retaining all ties in profile-layout order.
enumeratePureEquilibria ::
    (Eq strategy, Eq x, Eq y, Eq r) =>
    EquilibriumBudget ->
    FiniteOpenGame owner strategy x s y r ->
    OpenGameContext x y r ->
    Either EquilibriumError (EquilibriumReport strategy)
enumeratePureEquilibria budget game context = do
    if contextMatches game context then Right () else Left EquilibriumContextMismatch
    preflightEquilibrium budget profileCount profileCount
    let (equilibria, checks) = strictSelect (\profile -> requiredEither "equilibrium response" (bestResponse game context profile profile)) profiles
    checks `seq` forceSpine equilibria `seq` Right (report checks equilibria)
  where
    profiles = finiteSetValues (strategySchemaProfiles (openGameStrategySchema game))
    profileCount = naturalLength profiles
    report checks equilibria =
        EquilibriumReport
            { representedProfileCount = profileCount
            , performedRelationChecks = checks
            , configuredProfileLimit = equilibriumProfileLimit budget
            , configuredRelationLimit = equilibriumRelationLimit budget
            , equilibriumCompleted = True
            , equilibriumProfiles = equilibria
            }

-- | Enumerate every represented best-response pair in incumbent-major order.
enumerateBestResponses ::
    (Eq strategy, Eq x, Eq y, Eq r) =>
    EquilibriumBudget ->
    FiniteOpenGame owner strategy x s y r ->
    OpenGameContext x y r ->
    Either EquilibriumError (EquilibriumReport (strategy, strategy))
enumerateBestResponses budget game context = do
    if contextMatches game context then Right () else Left EquilibriumContextMismatch
    preflightEquilibrium budget profileCount pairCount
    let candidates = [(incumbent, deviation) | incumbent <- profiles, deviation <- profiles]
        (responses, checks) = strictSelect (\(incumbent, deviation) -> requiredEither "best-response enumeration" (bestResponse game context incumbent deviation)) candidates
    checks `seq`
        forceSpine responses `seq`
            Right
                EquilibriumReport
                    { representedProfileCount = profileCount
                    , performedRelationChecks = checks
                    , configuredProfileLimit = equilibriumProfileLimit budget
                    , configuredRelationLimit = equilibriumRelationLimit budget
                    , equilibriumCompleted = True
                    , equilibriumProfiles = responses
                    }
  where
    profiles = finiteSetValues (strategySchemaProfiles (openGameStrategySchema game))
    profileCount = naturalLength profiles
    pairCount = profileCount * profileCount

-- | Maximum aggregate work for exhaustive observational equality.
newtype ObservationBudget = ObservationBudget Natural
    deriving (Eq, Show)

-- | Set the exhaustive observation work limit.
observationBudget :: Natural -> ObservationBudget
observationBudget = ObservationBudget

-- | Boundary, ownership, bijection, or work failures during equality.
data ObservationError
    = ObservationBoundaryMismatch
    | ObservationOwnerMismatch
    | ObservationStrategyBijectionMismatch
    | ObservationWorkLimitExceeded !Natural !Natural
    | ObservationInternalConstructionFailure
    deriving (Eq, Show)

-- | Exhaustive represented-check report.
data ObservationReport = ObservationReport
    { observationRequiredWork :: !Natural
    , observationPerformedPlayChecks :: !Natural
    , observationPerformedCoplayChecks :: !Natural
    , observationPerformedBestResponseChecks :: !Natural
    , observationEquivalent :: !Bool
    }
    deriving (Eq, Show)

{- | Exhaustively compare play, coplay, and best-response membership under an
explicit owner-preserving strategy bijection.  This is finite represented
equality, not contextual equivalence for arbitrary carriers.
-}
observationallyEqualUnder ::
    (Eq owner, Eq leftStrategy, Eq rightStrategy, Eq x, Eq s, Eq y, Eq r) =>
    ObservationBudget ->
    StrategyBijection owner leftStrategy rightStrategy ->
    FiniteOpenGame owner leftStrategy x s y r ->
    FiniteOpenGame owner rightStrategy x s y r ->
    Either ObservationError ObservationReport
observationallyEqualUnder budget (StrategyBijection leftShape rightShape bijection) left right
    | not boundariesAgree = Left ObservationBoundaryMismatch
    | leftShape /= schemaShape (openGameStrategySchema left) || rightShape /= schemaShape (openGameStrategySchema right) = Left ObservationOwnerMismatch
    | not bijectionAgrees = Left ObservationStrategyBijectionMismatch
    | requiredWork > observationLimit budget = Left (ObservationWorkLimitExceeded requiredWork (observationLimit budget))
    | otherwise = do
        continuations <- mapLeft (const ObservationInternalConstructionFailure) (enumerateFiniteFunctions (finiteBudget requiredWork) (openGameTarget left) (openGameUtility left))
        let playResults = [playOpenGame left strategy input == playOpenGame right (mapStrategy strategy) input | strategy <- leftProfiles, input <- inputs]
            coplayResults = [coplayOpenGame left strategy input utility == coplayOpenGame right (mapStrategy strategy) input utility | strategy <- leftProfiles, input <- inputs, utility <- utilities]
            responseResults =
                [ response left strategy deviation input continuation
                    == response right (mapStrategy strategy) (mapStrategy deviation) input (relayout continuation)
                | input <- inputs
                , continuation <- continuations
                , strategy <- leftProfiles
                , deviation <- leftProfiles
                ]
            (playsAgree, actualPlayChecks) = strictAll playResults
            (coplaysAgree, actualCoplayChecks) = strictAll coplayResults
            (responsesAgree, actualResponseChecks) = strictAll responseResults
            equivalent = playsAgree && coplaysAgree && responsesAgree
        actualPlayChecks `seq`
            actualCoplayChecks `seq`
                actualResponseChecks `seq`
                    equivalent `seq`
                        Right
                            ObservationReport
                                { observationRequiredWork = requiredWork
                                , observationPerformedPlayChecks = actualPlayChecks
                                , observationPerformedCoplayChecks = actualCoplayChecks
                                , observationPerformedBestResponseChecks = actualResponseChecks
                                , observationEquivalent = equivalent
                                }
  where
    leftProfiles = finiteSetValues (strategySchemaProfiles (openGameStrategySchema left))
    rightProfiles = strategySchemaProfiles (openGameStrategySchema right)
    inputs = finiteSetValues (openGameSource left)
    utilities = finiteSetValues (openGameUtility left)
    strategyCount = naturalLength leftProfiles
    inputCount = cardinality (openGameSource left)
    utilityCount = cardinality (openGameUtility left)
    outputCount = cardinality (openGameTarget left)
    continuationCount = functionSpaceCardinality (openGameTarget left) (openGameUtility left)
    playChecks = strategyCount * inputCount
    coplayChecks = playChecks * utilityCount
    responseChecks = inputCount * continuationCount * strategyCount * strategyCount
    enumerationWork = continuationCount + continuationCount * outputCount
    requiredWork = playChecks + coplayChecks + responseChecks + enumerationWork
    boundariesAgree =
        sameFiniteSet (openGameSource left) (openGameSource right)
            && sameFiniteSet (openGameCoutility left) (openGameCoutility right)
            && sameFiniteSet (openGameTarget left) (openGameTarget right)
            && sameFiniteSet (openGameUtility left) (openGameUtility right)
    bijectionAgrees =
        sameFiniteSet (finiteBijectionLeft bijection) (strategySchemaProfiles (openGameStrategySchema left))
            && sameFiniteSet (finiteBijectionRight bijection) rightProfiles
    mapStrategy strategy = required "strategy bijection" (applyFiniteBijection bijection strategy)
    relayout continuation =
        requiredEither
            "continuation relayout"
            ( finiteFunction
                (finiteBudget requiredWork)
                (openGameTarget right)
                (openGameUtility right)
                (finiteFunctionEntries continuation)
            )
    response game strategy deviation input continuation =
        requiredEither
            "observational best response"
            (bestResponse game (OpenGameContext input continuation) strategy deviation)

{- | Compare represented owners, profiles, boundaries, play, and coplay layouts.
Best-response relations are intentionally not inferred from this diagnostic.
-}
sameOpenGameLayout ::
    (Eq owner, Eq strategy, Eq x, Eq s, Eq y, Eq r) =>
    FiniteOpenGame owner strategy x s y r ->
    FiniteOpenGame owner strategy x s y r ->
    Bool
sameOpenGameLayout left right =
    strategySchemaOwners (openGameStrategySchema left) == strategySchemaOwners (openGameStrategySchema right)
        && FiniteSet.sameFiniteLayout (strategySchemaProfiles (openGameStrategySchema left)) (strategySchemaProfiles (openGameStrategySchema right))
        && FiniteSet.sameFiniteLayout (openGameSource left) (openGameSource right)
        && FiniteSet.sameFiniteLayout (openGameCoutility left) (openGameCoutility right)
        && FiniteSet.sameFiniteLayout (openGameTarget left) (openGameTarget right)
        && FiniteSet.sameFiniteLayout (openGameUtility left) (openGameUtility right)
        && all (\strategy -> all (\input -> playOpenGame left strategy input == playOpenGame right strategy input) (finiteSetValues (openGameSource left))) (finiteSetValues (strategySchemaProfiles (openGameStrategySchema left)))
        && all (\strategy -> all (\(input, utility) -> coplayOpenGame left strategy input utility == coplayOpenGame right strategy input utility) [(input, utility) | input <- finiteSetValues (openGameSource left), utility <- finiteSetValues (openGameUtility left)]) (finiteSetValues (strategySchemaProfiles (openGameStrategySchema left)))

bestResponseEvaluator :: FiniteOpenGame owner strategy x s y r -> x -> FiniteFunction y r -> strategy -> strategy -> Bool
bestResponseEvaluator (FiniteOpenGame _ _ _ _ _ _ _ evaluator) = evaluator

combineSchemas ::
    (Eq owner, Eq firstStrategy, Eq secondStrategy) =>
    StrategySchema owner firstStrategy ->
    StrategySchema owner secondStrategy ->
    Either (OpenGameCompositionError owner) (StrategySchema owner (firstStrategy, secondStrategy))
combineSchemas first second =
    case find (`elem` secondOwners) firstOwners of
        Just duplicate -> Left (OverlappingStrategyOwner duplicate)
        Nothing -> do
            profiles <- internalProduct (strategySchemaProfiles first) (strategySchemaProfiles second)
            Right (StrategySchema (ProductSchemaShape (schemaShape first) (schemaShape second)) profiles)
  where
    firstOwners = strategySchemaOwners first
    secondOwners = strategySchemaOwners second

firstOverlap ::
    (Eq owner) =>
    FiniteOpenGame owner firstStrategy x s y r ->
    FiniteOpenGame owner secondStrategy x2 s2 y2 r2 ->
    Maybe owner
firstOverlap first second =
    find (`elem` secondOwners) firstOwners
  where
    firstOwners = strategySchemaOwners (openGameStrategySchema first)
    secondOwners = strategySchemaOwners (openGameStrategySchema second)

mapConstructionError ::
    FiniteBudget ->
    Either (FiniteOpenGameError strategy x s y r) value ->
    Either (OpenGameCompositionError owner) value
mapConstructionError _ (Right value) = Right value
mapConstructionError _ (Left (OpenGameWorkLimitExceeded requiredWork limit)) = Left (OpenGameCompositionWorkLimitExceeded requiredWork limit)
mapConstructionError _ (Left _) = Left OpenGameInternalConstructionFailure

contextMatches :: (Eq x, Eq y, Eq r) => FiniteOpenGame owner strategy x s y r -> OpenGameContext x y r -> Bool
contextMatches game context =
    contextInput context `elem` finiteSetValues (openGameSource game)
        && sameFiniteSet (finiteFunctionSource (contextContinuation context)) (openGameTarget game)
        && sameFiniteSet (finiteFunctionTarget (contextContinuation context)) (openGameUtility game)

preflightEquilibrium :: EquilibriumBudget -> Natural -> Natural -> Either EquilibriumError ()
preflightEquilibrium budget profileCount relationCount
    | profileCount > equilibriumProfileLimit budget = Left (EquilibriumProfileLimitExceeded profileCount (equilibriumProfileLimit budget))
    | relationCount > equilibriumRelationLimit budget = Left (EquilibriumRelationLimitExceeded relationCount (equilibriumRelationLimit budget))
    | otherwise = Right ()

equilibriumProfileLimit :: EquilibriumBudget -> Natural
equilibriumProfileLimit (EquilibriumBudget limit _) = limit

equilibriumRelationLimit :: EquilibriumBudget -> Natural
equilibriumRelationLimit (EquilibriumBudget _ limit) = limit

observationLimit :: ObservationBudget -> Natural
observationLimit (ObservationBudget limit) = limit

internalProduct :: (Eq left, Eq right) => FiniteSet left -> FiniteSet right -> Either (OpenGameCompositionError owner) (FiniteSet (left, right))
internalProduct = productSet

productSet :: (Eq left, Eq right) => FiniteSet left -> FiniteSet right -> Either error (FiniteSet (left, right))
productSet left right =
    case finiteSet [(leftValue, rightValue) | leftValue <- finiteSetValues left, rightValue <- finiteSetValues right] of
        Right productObject -> Right productObject
        Left _ -> error "productSet: products of duplicate-free carriers must be duplicate-free"

tripleSet :: (Eq first, Eq second, Eq third) => FiniteSet first -> FiniteSet second -> FiniteSet third -> Either error (FiniteSet (first, second, third))
tripleSet first second third =
    case finiteSet [(firstValue, secondValue, thirdValue) | firstValue <- finiteSetValues first, secondValue <- finiteSetValues second, thirdValue <- finiteSetValues third] of
        Right productObject -> Right productObject
        Left _ -> error "tripleSet: products of duplicate-free carriers must be duplicate-free"

singletonUnit :: FiniteSet ()
singletonUnit = case finiteSet [()] of
    Right result -> result
    Left _ -> error "singleton finite set must be valid"

strictAll :: [Bool] -> (Bool, Natural)
strictAll = foldl' step (True, 0)
  where
    step (result, count) value =
        value `seq` let next = result && value in next `seq` (next, count + 1)

strictSelect :: (value -> Bool) -> [value] -> ([value], Natural)
strictSelect predicate = go [] 0
  where
    go selected count [] = (reverse selected, count)
    go selected count (value : values) =
        let accepted = predicate value
            next = if accepted then value : selected else selected
         in accepted `seq` go next (count + 1) values

forceSpine :: [value] -> ()
forceSpine [] = ()
forceSpine (_ : values) = forceSpine values

cardinality :: FiniteSet value -> Natural
cardinality = fromIntegral . finiteSetCardinality

naturalLength :: [value] -> Natural
naturalLength = fromIntegral . length

applyRequired :: (Eq input) => FiniteFunction input output -> input -> output
applyRequired function input = required "finite continuation" (applyFiniteFunction function input)

required :: String -> Maybe value -> value
required _ (Just value) = value
required label Nothing = error ("checked finite open-game lookup failed: " ++ label)

requiredEither :: String -> Either error value -> value
requiredEither _ (Right value) = value
requiredEither label (Left _) = error ("checked finite open-game invariant failed: " ++ label)

mapLeft :: (left -> other) -> Either left value -> Either other value
mapLeft operation (Left problem) = Left (operation problem)
mapLeft _ (Right value) = Right value
