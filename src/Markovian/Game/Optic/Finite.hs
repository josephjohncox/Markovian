{-# LANGUAGE RoleAnnotations #-}

{- | Checked finite functions, bijections, and concrete cartesian optics.

A 'FiniteOptic' is only a represented pair @P : X -> Y@ and
@C : X * R -> S@.  It is not the general residual/coend construction of
optics.  Every table allocation and function-space enumeration has an
explicit preflight bound.
-}
module Markovian.Game.Optic.Finite (
    FiniteBudget,
    finiteBudget,
    maximumFiniteWork,
    FiniteFunction,
    FiniteFunctionError (..),
    finiteFunction,
    finiteFunctionFromFunction,
    finiteFunctionSource,
    finiteFunctionTarget,
    finiteFunctionEntries,
    applyFiniteFunction,
    finiteFunctionEquivalent,
    sameFiniteFunctionLayout,
    functionSpaceCardinality,
    enumerateFiniteFunctions,
    FiniteBijection,
    FiniteBijectionError (..),
    finiteBijection,
    identityFiniteBijection,
    finiteBijectionLeft,
    finiteBijectionRight,
    applyFiniteBijection,
    applyInverseFiniteBijection,
    FiniteOptic,
    FiniteOpticError (..),
    FiniteOpticCompositionError (..),
    finiteOptic,
    finiteOpticSource,
    finiteOpticCoutility,
    finiteOpticTarget,
    finiteOpticUtility,
    opticPlay,
    opticCoplay,
    identityFiniteOptic,
    composeFiniteOptic,
    tensorFiniteOptic,
    finiteOpticEquivalent,
    sameFiniteOpticLayout,
) where

import Data.List (find)
import Markovian.Category.Finite.Set (FiniteSet, finiteSet, finiteSetCardinality, finiteSetValues, sameFiniteSet)
import Markovian.Category.Finite.Set qualified as FiniteSet
import Numeric.Natural (Natural)

-- | Maximum aggregate represented table work.  Zero is a valid rejecting bound.
newtype FiniteBudget = FiniteBudget Natural
    deriving (Eq, Show)

-- | Set the maximum aggregate represented work.
finiteBudget :: Natural -> FiniteBudget
finiteBudget = FiniteBudget

-- | Read the configured represented-work limit.
maximumFiniteWork :: FiniteBudget -> Natural
maximumFiniteWork (FiniteBudget maximumWork) = maximumWork

-- | A total table between explicit finite carriers.
data FiniteFunction input output = FiniteFunction !(FiniteSet input) !(FiniteSet output) ![(input, output)]

type role FiniteFunction nominal nominal

instance (Eq input, Eq output) => Eq (FiniteFunction input output) where
    left == right = sameFiniteFunctionLayout left right

instance (Show input, Show output) => Show (FiniteFunction input output) where
    showsPrec precedence function =
        showParen (precedence > 10) $
            showString "FiniteFunction " . shows (finiteFunctionEntries function)

-- | Checked-table or bounded-enumeration failures.
data FiniteFunctionError input output
    = FiniteFunctionWorkLimitExceeded !Natural !Natural
    | DuplicateFunctionInput !input
    | FunctionInputOutsideSource !input
    | FunctionOutputOutsideTarget !input !output
    | MissingFunctionInput !input
    | ExcessFunctionEntries
    deriving (Eq, Show)

-- | Validate a total table.  Input order is canonicalized to source-layout order.
finiteFunction ::
    (Eq input, Eq output) =>
    FiniteBudget ->
    FiniteSet input ->
    FiniteSet output ->
    [(input, output)] ->
    Either (FiniteFunctionError input output) (FiniteFunction input output)
finiteFunction budget source target supplied = do
    requireWork budget requiredEntries
    let bounded = takeAtMost (requiredEntries + 1) supplied
    if naturalLength bounded > requiredEntries
        then Left ExcessFunctionEntries
        else case firstDuplicate (map fst bounded) of
            Just duplicate -> Left (DuplicateFunctionInput duplicate)
            Nothing -> case find ((`notElem` finiteSetValues source) . fst) bounded of
                Just (outside, _) -> Left (FunctionInputOutsideSource outside)
                Nothing -> case find (\(_, output) -> output `notElem` finiteSetValues target) bounded of
                    Just (input, outside) -> Left (FunctionOutputOutsideTarget input outside)
                    Nothing -> case find (`notElem` map fst bounded) (finiteSetValues source) of
                        Just missing -> Left (MissingFunctionInput missing)
                        Nothing ->
                            Right
                                ( FiniteFunction
                                    source
                                    target
                                    [(input, outputAt input bounded) | input <- finiteSetValues source]
                                )
  where
    requiredEntries = cardinality source
    outputAt input entries = case lookup input entries of
        Just output -> output
        Nothing -> error "finiteFunction: checked total table lost an entry"

-- | Build a total table after checking its represented size before evaluation.
finiteFunctionFromFunction ::
    (Eq input, Eq output) =>
    FiniteBudget ->
    FiniteSet input ->
    FiniteSet output ->
    (input -> output) ->
    Either (FiniteFunctionError input output) (FiniteFunction input output)
finiteFunctionFromFunction budget source target function =
    finiteFunction budget source target [(input, function input) | input <- finiteSetValues source]

-- | Read the represented source carrier.
finiteFunctionSource :: FiniteFunction input output -> FiniteSet input
finiteFunctionSource (FiniteFunction source _ _) = source

-- | Read the represented target carrier.
finiteFunctionTarget :: FiniteFunction input output -> FiniteSet output
finiteFunctionTarget (FiniteFunction _ target _) = target

-- | Read entries in source-layout order.
finiteFunctionEntries :: FiniteFunction input output -> [(input, output)]
finiteFunctionEntries (FiniteFunction _ _ entries) = entries

-- | Apply a represented function.  Inputs outside its source return 'Nothing'.
applyFiniteFunction :: (Eq input) => FiniteFunction input output -> input -> Maybe output
applyFiniteFunction (FiniteFunction _ _ entries) input = lookup input entries

-- | Compare labelled source, target, and function values while ignoring layout.
finiteFunctionEquivalent :: (Eq input, Eq output) => FiniteFunction input output -> FiniteFunction input output -> Bool
finiteFunctionEquivalent left right =
    sameFiniteSet (finiteFunctionSource left) (finiteFunctionSource right)
        && sameFiniteSet (finiteFunctionTarget left) (finiteFunctionTarget right)
        && all (\(input, output) -> applyFiniteFunction right input == Just output) (finiteFunctionEntries left)

-- | Compare labelled semantics and represented source, target, and row order.
sameFiniteFunctionLayout :: (Eq input, Eq output) => FiniteFunction input output -> FiniteFunction input output -> Bool
sameFiniteFunctionLayout left right =
    finiteFunctionEquivalent left right
        && FiniteSet.sameFiniteLayout (finiteFunctionSource left) (finiteFunctionSource right)
        && FiniteSet.sameFiniteLayout (finiteFunctionTarget left) (finiteFunctionTarget right)
        && finiteFunctionEntries left == finiteFunctionEntries right

-- | Number of represented total functions, including @0^0 = 1@.
functionSpaceCardinality :: FiniteSet input -> FiniteSet output -> Natural
functionSpaceCardinality source target = cardinality target ^ finiteSetCardinality source

-- | Enumerate all total functions in source-major, target-layout lexicographic order.
enumerateFiniteFunctions ::
    (Eq input, Eq output) =>
    FiniteBudget ->
    FiniteSet input ->
    FiniteSet output ->
    Either (FiniteFunctionError input output) [FiniteFunction input output]
enumerateFiniteFunctions budget source target = do
    requireWork budget requiredWork
    traverse (finiteFunction budget source target . zip (finiteSetValues source)) assignments
  where
    assignments = sequencesOf (finiteSetCardinality source) (finiteSetValues target)
    count = functionSpaceCardinality source target
    requiredWork = count + count * cardinality source

-- | A checked represented bijection and its inverse table.
data FiniteBijection left right = FiniteBijection !(FiniteSet left) !(FiniteSet right) ![(left, right)]

type role FiniteBijection nominal nominal

-- | Bijection construction failures.
data FiniteBijectionError left right
    = FiniteBijectionFunctionError !(FiniteFunctionError left right)
    | DuplicateBijectionImage !right
    | MissingBijectionImage !right
    deriving (Eq, Show)

-- | Validate a total one-to-one and onto table.
finiteBijection ::
    (Eq left, Eq right) =>
    FiniteBudget ->
    FiniteSet left ->
    FiniteSet right ->
    [(left, right)] ->
    Either (FiniteBijectionError left right) (FiniteBijection left right)
finiteBijection budget left right entries = do
    function <- mapLeft FiniteBijectionFunctionError (finiteFunction budget left right entries)
    case firstDuplicate (map snd (finiteFunctionEntries function)) of
        Just duplicate -> Left (DuplicateBijectionImage duplicate)
        Nothing -> case find (`notElem` map snd (finiteFunctionEntries function)) (finiteSetValues right) of
            Just missing -> Left (MissingBijectionImage missing)
            Nothing -> Right (FiniteBijection left right (finiteFunctionEntries function))

-- | Construct the represented identity bijection.
identityFiniteBijection :: (Eq value) => FiniteBudget -> FiniteSet value -> Either (FiniteBijectionError value value) (FiniteBijection value value)
identityFiniteBijection budget object = finiteBijection budget object object [(value, value) | value <- finiteSetValues object]

-- | Read the represented left carrier.
finiteBijectionLeft :: FiniteBijection left right -> FiniteSet left
finiteBijectionLeft (FiniteBijection left _ _) = left

-- | Read the represented right carrier.
finiteBijectionRight :: FiniteBijection left right -> FiniteSet right
finiteBijectionRight (FiniteBijection _ right _) = right

-- | Apply the forward table to a represented left value.
applyFiniteBijection :: (Eq left) => FiniteBijection left right -> left -> Maybe right
applyFiniteBijection (FiniteBijection _ _ entries) value = lookup value entries

-- | Apply the checked inverse table to a represented right value.
applyInverseFiniteBijection :: (Eq right) => FiniteBijection left right -> right -> Maybe left
applyInverseFiniteBijection (FiniteBijection _ _ entries) value = lookup value [(right, left) | (left, right) <- entries]

-- | A concrete checked pair @X -> Y@ and @X * R -> S@.
data FiniteOptic x s y r
    = CheckedFiniteOptic
        !(FiniteSet x)
        !(FiniteSet s)
        !(FiniteSet y)
        !(FiniteSet r)
        !(FiniteFunction x y)
        !(FiniteFunction (x, r) s)

type role FiniteOptic nominal nominal nominal nominal

-- | Construction failures for a concrete optic.
data FiniteOpticError x s y r
    = OpticWorkLimitExceeded !Natural !Natural
    | OpticPlayTableError !(FiniteFunctionError x y)
    | OpticCoplayTableError !(FiniteFunctionError (x, r) s)
    deriving (Eq, Show)

-- | Composition boundary or allocation failure.
data FiniteOpticCompositionError
    = OpticForwardBoundaryMismatch
    | OpticBackwardBoundaryMismatch
    | OpticCompositionWorkLimitExceeded !Natural !Natural
    | OpticInternalConstructionFailure
    deriving (Eq, Show)

-- | Validate complete play and coplay tables under one aggregate bound.
finiteOptic ::
    (Eq x, Eq s, Eq y, Eq r) =>
    FiniteBudget ->
    FiniteSet x ->
    FiniteSet s ->
    FiniteSet y ->
    FiniteSet r ->
    [(x, y)] ->
    [((x, r), s)] ->
    Either (FiniteOpticError x s y r) (FiniteOptic x s y r)
finiteOptic budget x s y r playEntries coplayEntries = do
    requireOpticWork budget (cardinality x + cardinality x * cardinality r)
    play <- mapLeft OpticPlayTableError (finiteFunction budget x y playEntries)
    xr <- productSet x r
    coplay <- mapLeft OpticCoplayTableError (finiteFunction budget xr s coplayEntries)
    Right (CheckedFiniteOptic x s y r play coplay)

-- | Read the forward source carrier.
finiteOpticSource :: FiniteOptic x s y r -> FiniteSet x
finiteOpticSource (CheckedFiniteOptic x _ _ _ _ _) = x

-- | Read the backward result carrier.
finiteOpticCoutility :: FiniteOptic x s y r -> FiniteSet s
finiteOpticCoutility (CheckedFiniteOptic _ s _ _ _ _) = s

-- | Read the forward target carrier.
finiteOpticTarget :: FiniteOptic x s y r -> FiniteSet y
finiteOpticTarget (CheckedFiniteOptic _ _ y _ _ _) = y

-- | Read the incoming utility carrier.
finiteOpticUtility :: FiniteOptic x s y r -> FiniteSet r
finiteOpticUtility (CheckedFiniteOptic _ _ _ r _ _) = r

-- | Apply the forward table.
opticPlay :: (Eq x) => FiniteOptic x s y r -> x -> Maybe y
opticPlay (CheckedFiniteOptic _ _ _ _ play _) = applyFiniteFunction play

-- | Apply the backward table to retained input and incoming utility.
opticCoplay :: (Eq x, Eq r) => FiniteOptic x s y r -> x -> r -> Maybe s
opticCoplay (CheckedFiniteOptic _ _ _ _ _ coplay) x r = applyFiniteFunction coplay (x, r)

-- | Construct the concrete identity optic.
identityFiniteOptic :: (Eq x, Eq s) => FiniteBudget -> FiniteSet x -> FiniteSet s -> Either (FiniteOpticError x s x s) (FiniteOptic x s x s)
identityFiniteOptic budget x s =
    finiteOptic budget x s x s [(value, value) | value <- finiteSetValues x] [((value, utility), utility) | value <- finiteSetValues x, utility <- finiteSetValues s]

-- | Compose concrete optics after labelled boundary checks.
composeFiniteOptic ::
    (Eq x, Eq s, Eq y, Eq r, Eq z, Eq q) =>
    FiniteBudget ->
    FiniteOptic x s y r ->
    FiniteOptic y r z q ->
    Either FiniteOpticCompositionError (FiniteOptic x s z q)
composeFiniteOptic budget first second
    | not (sameFiniteSet (finiteOpticTarget first) (finiteOpticSource second)) = Left OpticForwardBoundaryMismatch
    | not (sameFiniteSet (finiteOpticUtility first) (finiteOpticCoutility second)) = Left OpticBackwardBoundaryMismatch
    | requiredWork > maximumFiniteWork budget = Left (OpticCompositionWorkLimitExceeded requiredWork (maximumFiniteWork budget))
    | otherwise =
        mapOpticCompositionError $
            finiteOptic budget x s z q playEntries coplayEntries
  where
    x = finiteOpticSource first
    s = finiteOpticCoutility first
    z = finiteOpticTarget second
    q = finiteOpticUtility second
    requiredWork = cardinality x + cardinality x * cardinality q
    playEntries = [(input, required "first play" (opticPlay second (required "second source" (opticPlay first input)))) | input <- finiteSetValues x]
    coplayEntries =
        [ ((input, utility), required "first coplay" (opticCoplay first input (required "second coplay" (opticCoplay second (required "middle play" (opticPlay first input)) utility))))
        | input <- finiteSetValues x
        , utility <- finiteSetValues q
        ]

-- | Tensor two concrete optics using cartesian products.
tensorFiniteOptic ::
    (Eq x1, Eq s1, Eq y1, Eq r1, Eq x2, Eq s2, Eq y2, Eq r2) =>
    FiniteBudget ->
    FiniteOptic x1 s1 y1 r1 ->
    FiniteOptic x2 s2 y2 r2 ->
    Either FiniteOpticCompositionError (FiniteOptic (x1, x2) (s1, s2) (y1, y2) (r1, r2))
tensorFiniteOptic budget first second
    | requiredWork > maximumFiniteWork budget = Left (OpticCompositionWorkLimitExceeded requiredWork (maximumFiniteWork budget))
    | otherwise = do
        x <- mapLeft (const OpticInternalConstructionFailure) (productSet (finiteOpticSource first) (finiteOpticSource second))
        s <- mapLeft (const OpticInternalConstructionFailure) (productSet (finiteOpticCoutility first) (finiteOpticCoutility second))
        y <- mapLeft (const OpticInternalConstructionFailure) (productSet (finiteOpticTarget first) (finiteOpticTarget second))
        r <- mapLeft (const OpticInternalConstructionFailure) (productSet (finiteOpticUtility first) (finiteOpticUtility second))
        mapOpticCompositionError $
            finiteOptic
                budget
                x
                s
                y
                r
                [ (input, (required "tensor first play" (opticPlay first (fst input)), required "tensor second play" (opticPlay second (snd input))))
                | input <- finiteSetValues x
                ]
                [ ( (input, utility)
                  ,
                      ( required "tensor first coplay" (opticCoplay first (fst input) (fst utility))
                      , required "tensor second coplay" (opticCoplay second (snd input) (snd utility))
                      )
                  )
                | input <- finiteSetValues x
                , utility <- finiteSetValues r
                ]
  where
    xCount = cardinality (finiteOpticSource first) * cardinality (finiteOpticSource second)
    sCount = cardinality (finiteOpticCoutility first) * cardinality (finiteOpticCoutility second)
    yCount = cardinality (finiteOpticTarget first) * cardinality (finiteOpticTarget second)
    rCount = cardinality (finiteOpticUtility first) * cardinality (finiteOpticUtility second)
    requiredWork = xCount + sCount + yCount + rCount + xCount + xCount * rCount

mapOpticCompositionError :: Either (FiniteOpticError x s y r) value -> Either FiniteOpticCompositionError value
mapOpticCompositionError (Right value) = Right value
mapOpticCompositionError (Left (OpticWorkLimitExceeded requiredWork limit)) = Left (OpticCompositionWorkLimitExceeded requiredWork limit)
mapOpticCompositionError (Left _) = Left OpticInternalConstructionFailure

-- | Compare labelled play and coplay semantics while ignoring layout.
finiteOpticEquivalent ::
    (Eq x, Eq s, Eq y, Eq r) =>
    FiniteOptic x s y r ->
    FiniteOptic x s y r ->
    Bool
finiteOpticEquivalent left right =
    sameFiniteSet (finiteOpticSource left) (finiteOpticSource right)
        && sameFiniteSet (finiteOpticCoutility left) (finiteOpticCoutility right)
        && sameFiniteSet (finiteOpticTarget left) (finiteOpticTarget right)
        && sameFiniteSet (finiteOpticUtility left) (finiteOpticUtility right)
        && all (\x -> opticPlay left x == opticPlay right x) (finiteSetValues (finiteOpticSource left))
        && all (\(x, r) -> opticCoplay left x r == opticCoplay right x r) [(x, r) | x <- finiteSetValues (finiteOpticSource left), r <- finiteSetValues (finiteOpticUtility left)]

-- | Compare labelled semantics and all represented boundary layouts.
sameFiniteOpticLayout ::
    (Eq x, Eq s, Eq y, Eq r) =>
    FiniteOptic x s y r ->
    FiniteOptic x s y r ->
    Bool
sameFiniteOpticLayout left right =
    finiteOpticEquivalent left right
        && FiniteSet.sameFiniteLayout (finiteOpticSource left) (finiteOpticSource right)
        && FiniteSet.sameFiniteLayout (finiteOpticCoutility left) (finiteOpticCoutility right)
        && FiniteSet.sameFiniteLayout (finiteOpticTarget left) (finiteOpticTarget right)
        && FiniteSet.sameFiniteLayout (finiteOpticUtility left) (finiteOpticUtility right)

productSet :: (Eq left, Eq right) => FiniteSet left -> FiniteSet right -> Either error (FiniteSet (left, right))
productSet left right =
    case finiteSet [(leftValue, rightValue) | leftValue <- finiteSetValues left, rightValue <- finiteSetValues right] of
        Right productObject -> Right productObject
        Left _ -> error "productSet: products of duplicate-free carriers must be duplicate-free"

requireWork :: FiniteBudget -> Natural -> Either (FiniteFunctionError input output) ()
requireWork budget requiredWork
    | requiredWork > maximumFiniteWork budget = Left (FiniteFunctionWorkLimitExceeded requiredWork (maximumFiniteWork budget))
    | otherwise = Right ()

requireOpticWork :: FiniteBudget -> Natural -> Either (FiniteOpticError x s y r) ()
requireOpticWork budget requiredWork
    | requiredWork > maximumFiniteWork budget = Left (OpticWorkLimitExceeded requiredWork (maximumFiniteWork budget))
    | otherwise = Right ()

cardinality :: FiniteSet value -> Natural
cardinality = fromIntegral . finiteSetCardinality

naturalLength :: [value] -> Natural
naturalLength = fromIntegral . length

takeAtMost :: Natural -> [value] -> [value]
takeAtMost 0 _ = []
takeAtMost _ [] = []
takeAtMost remaining (value : values) = value : takeAtMost (remaining - 1) values

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate [] = Nothing
firstDuplicate (value : remaining)
    | value `elem` remaining = Just value
    | otherwise = firstDuplicate remaining

sequencesOf :: Int -> [value] -> [[value]]
sequencesOf 0 _ = [[]]
sequencesOf amount values = [value : rest | value <- values, rest <- sequencesOf (amount - 1) values]

required :: String -> Maybe value -> value
required _ (Just value) = value
required label Nothing = error ("checked finite optic lookup failed: " ++ label)

mapLeft :: (left -> other) -> Either left value -> Either other value
mapLeft operation (Left problem) = Left (operation problem)
mapLeft _ (Right value) = Right value
