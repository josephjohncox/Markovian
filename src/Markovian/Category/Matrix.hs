{-# LANGUAGE GADTs #-}

{- | Opaque source-by-target finite semiring matrices.

Composition is written left to right. Matrix semantics are indexed by represented
values; storage order is not semantic. Empty source and target sets are valid.
-}
module Markovian.Category.Matrix (
    Matrix,
    MatrixError (..),
    matrixFromFunction,
    matrixFromRows,
    matrixSource,
    matrixTarget,
    matrixRows,
    matrixEntry,
    matrixEquivalent,
    sameMatrixLayout,
    identityMatrix,
    composeMatrix,
    zeroMatrix,
    addMatrix,
    scaleMatrix,
    tensorMatrix,
    reindexMatrix,
    transposeMatrix,
    conjugateTransposeMatrix,
    directSumMatrix,
    leftInjectionMatrix,
    rightInjectionMatrix,
    leftProjectionMatrix,
    rightProjectionMatrix,
    cupMatrix,
    capMatrix,
    traceMatrix,
) where

import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Markovian.Algebra.Semiring
import Markovian.Category.Finite.Set
import Markovian.Category.Finite.Set.Internal (FiniteSet (UnsafeFiniteSet))

-- | A checked source-by-target, row-major matrix.
data Matrix scalar source target where
    Matrix ::
        (Eq source, Eq target) =>
        !(FiniteSet source) ->
        !(FiniteSet target) ->
        ![scalar] ->
        Matrix scalar source target

-- | Matrix construction or object-alignment failure.
data MatrixError
    = MatrixRowCountMismatch !Int !Int
    | MatrixColumnCountMismatch !Int !Int !Int
    | MatrixMiddleObjectMismatch
    | MatrixSourceObjectMismatch
    | MatrixTargetObjectMismatch
    | MatrixTraceObjectMismatch
    deriving (Eq, Show)

-- | Construct a matrix by total value-indexed evaluation.
matrixFromFunction ::
    FiniteSet source ->
    FiniteSet target ->
    (source -> target -> scalar) ->
    Matrix scalar source target
matrixFromFunction source@(UnsafeFiniteSet _) target@(UnsafeFiniteSet _) entry =
    Matrix
        source
        target
        [ entry sourceValue targetValue
        | sourceValue <- finiteSetValues source
        , targetValue <- finiteSetValues target
        ]

-- | Construct a matrix from checked source rows.
matrixFromRows ::
    FiniteSet source ->
    FiniteSet target ->
    [[scalar]] ->
    Either MatrixError (Matrix scalar source target)
matrixFromRows source@(UnsafeFiniteSet _) target@(UnsafeFiniteSet _) rows
    | actualRows /= expectedRows = Left (MatrixRowCountMismatch expectedRows actualRows)
    | otherwise =
        case firstBadRow 0 rows of
            Just (rowIndex, actualColumns) ->
                Left (MatrixColumnCountMismatch rowIndex expectedColumns actualColumns)
            Nothing -> Right (Matrix source target (concat rows))
  where
    expectedRows = finiteSetCardinality source
    actualRows = length rows
    expectedColumns = finiteSetCardinality target
    firstBadRow _ [] = Nothing
    firstBadRow rowIndex (row : remaining)
        | length row == expectedColumns = firstBadRow (rowIndex + 1) remaining
        | otherwise = Just (rowIndex, length row)

-- | Read the source witness.
matrixSource :: Matrix scalar source target -> FiniteSet source
matrixSource (Matrix source _ _) = source

-- | Read the target witness.
matrixTarget :: Matrix scalar source target -> FiniteSet target
matrixTarget (Matrix _ target _) = target

-- | Read checked rows in layout order.
matrixRows :: Matrix scalar source target -> [[scalar]]
matrixRows (Matrix source target entries) =
    rowsOf (finiteSetCardinality source) (finiteSetCardinality target) entries

-- | Total lookup. Values outside either represented support return 'Nothing'.
matrixEntry :: Matrix scalar source target -> source -> target -> Maybe scalar
matrixEntry matrix@(Matrix{}) sourceValue targetValue =
    lookup (sourceValue, targetValue) (labelledEntries matrix)

-- | Compare supports and entries by represented labels, ignoring layout order.
matrixEquivalent ::
    (Eq scalar) =>
    Matrix scalar source target ->
    Matrix scalar source target ->
    Bool
matrixEquivalent left right =
    sameFiniteSet (matrixSource left) (matrixSource right)
        && sameFiniteSet (matrixTarget left) (matrixTarget right)
        && and
            [ matrixEntry left sourceValue targetValue
                == matrixEntry right sourceValue targetValue
            | sourceValue <- finiteSetValues (matrixSource left)
            , targetValue <- finiteSetValues (matrixTarget left)
            ]

-- | Compare support layout and row-major representation literally.
sameMatrixLayout ::
    (Eq scalar) =>
    Matrix scalar source target ->
    Matrix scalar source target ->
    Bool
sameMatrixLayout (Matrix leftSource leftTarget leftEntries) (Matrix rightSource rightTarget rightEntries) =
    sameFiniteSetLayout leftSource rightSource
        && sameFiniteSetLayout leftTarget rightTarget
        && leftEntries == rightEntries

-- | Identity on an explicit finite set.
identityMatrix :: (Semiring scalar) => FiniteSet value -> Matrix scalar value value
identityMatrix object@(UnsafeFiniteSet _) =
    matrixFromFunction object object $ \sourceValue targetValue ->
        if sourceValue == targetValue then one else zero

-- | Left-to-right matrix composition with semantic middle-object reindexing.
composeMatrix ::
    (Semiring scalar) =>
    Matrix scalar source middle ->
    Matrix scalar middle target ->
    Either MatrixError (Matrix scalar source target)
composeMatrix left right
    | not (sameFiniteSet (matrixTarget left) (matrixSource right)) =
        Left MatrixMiddleObjectMismatch
    | otherwise =
        Right $ matrixFromFunction (matrixSource left) (matrixTarget right) $ \sourceValue targetValue ->
            foldl'
                plus
                zero
                [ leftEntry `times` rightEntry
                | middleValue <- finiteSetValues (matrixTarget left)
                , Just leftEntry <- [matrixEntry left sourceValue middleValue]
                , Just rightEntry <- [matrixEntry right middleValue targetValue]
                ]

-- | Pointwise zero on explicit objects.
zeroMatrix :: (Semiring scalar) => FiniteSet source -> FiniteSet target -> Matrix scalar source target
zeroMatrix source target = matrixFromFunction source target (\_ _ -> zero)

-- | Pointwise matrix addition after semantic object checks.
addMatrix ::
    (Semiring scalar) =>
    Matrix scalar source target ->
    Matrix scalar source target ->
    Either MatrixError (Matrix scalar source target)
addMatrix left right
    | not (sameFiniteSet (matrixSource left) (matrixSource right)) = Left MatrixSourceObjectMismatch
    | not (sameFiniteSet (matrixTarget left) (matrixTarget right)) = Left MatrixTargetObjectMismatch
    | otherwise =
        Right $ matrixFromFunction (matrixSource left) (matrixTarget left) $ \sourceValue targetValue ->
            valueOrZero left sourceValue targetValue `plus` valueOrZero right sourceValue targetValue

-- | Multiply every entry by one scalar.
scaleMatrix ::
    (CommutativeSemiring scalar) =>
    scalar ->
    Matrix scalar source target ->
    Matrix scalar source target
scaleMatrix scalar matrix =
    matrixFromFunction (matrixSource matrix) (matrixTarget matrix) $ \sourceValue targetValue ->
        scalar `times` valueOrZero matrix sourceValue targetValue

-- | Kronecker tensor. Product layout is left-major.
tensorMatrix ::
    (CommutativeSemiring scalar) =>
    Matrix scalar leftSource leftTarget ->
    Matrix scalar rightSource rightTarget ->
    Matrix scalar (leftSource, rightSource) (leftTarget, rightTarget)
tensorMatrix left right =
    matrixFromFunction source target $ \(leftSource, rightSource) (leftTarget, rightTarget) ->
        valueOrZero left leftSource leftTarget `times` valueOrZero right rightSource rightTarget
  where
    source = productSet (matrixSource left) (matrixSource right)
    target = productSet (matrixTarget left) (matrixTarget right)

-- | Change storage layouts without changing labelled entries.
reindexMatrix ::
    (Semiring scalar) =>
    FiniteSet source ->
    FiniteSet target ->
    Matrix scalar source target ->
    Either MatrixError (Matrix scalar source target)
reindexMatrix source target matrix
    | not (sameFiniteSet source (matrixSource matrix)) = Left MatrixSourceObjectMismatch
    | not (sameFiniteSet target (matrixTarget matrix)) = Left MatrixTargetObjectMismatch
    | otherwise =
        Right $ matrixFromFunction source target $ \sourceValue targetValue ->
            valueOrZero matrix sourceValue targetValue

-- | Matrix transpose.
transposeMatrix :: (Semiring scalar) => Matrix scalar source target -> Matrix scalar target source
transposeMatrix matrix =
    matrixFromFunction (matrixTarget matrix) (matrixSource matrix) $ \targetValue sourceValue ->
        valueOrZero matrix sourceValue targetValue

-- | Matrix conjugate transpose for an involutive scalar.
conjugateTransposeMatrix ::
    (InvolutiveSemiring scalar) =>
    Matrix scalar source target ->
    Matrix scalar target source
conjugateTransposeMatrix matrix =
    matrixFromFunction (matrixTarget matrix) (matrixSource matrix) $ \targetValue sourceValue ->
        involute (valueOrZero matrix sourceValue targetValue)

-- | Block-diagonal biproduct of two matrices. This is not tensor.
directSumMatrix ::
    (Semiring scalar) =>
    Matrix scalar leftSource leftTarget ->
    Matrix scalar rightSource rightTarget ->
    Matrix scalar (Either leftSource rightSource) (Either leftTarget rightTarget)
directSumMatrix left right =
    matrixFromFunction source target entry
  where
    source = sumSet (matrixSource left) (matrixSource right)
    target = sumSet (matrixTarget left) (matrixTarget right)
    entry (Left sourceValue) (Left targetValue) = valueOrZero left sourceValue targetValue
    entry (Right sourceValue) (Right targetValue) = valueOrZero right sourceValue targetValue
    entry _ _ = zero

-- | Left biproduct injection.
leftInjectionMatrix ::
    (Semiring scalar) =>
    FiniteSet left ->
    FiniteSet right ->
    Matrix scalar left (Either left right)
leftInjectionMatrix left@(UnsafeFiniteSet _) right =
    matrixFromFunction left (sumSet left right) $ \sourceValue targetValue ->
        case targetValue of
            Left value | sourceValue == value -> one
            _ -> zero

-- | Right biproduct injection.
rightInjectionMatrix ::
    (Semiring scalar) =>
    FiniteSet left ->
    FiniteSet right ->
    Matrix scalar right (Either left right)
rightInjectionMatrix left right@(UnsafeFiniteSet _) =
    matrixFromFunction right (sumSet left right) $ \sourceValue targetValue ->
        case targetValue of
            Right value | sourceValue == value -> one
            _ -> zero

-- | Left biproduct projection.
leftProjectionMatrix ::
    (Semiring scalar) =>
    FiniteSet left ->
    FiniteSet right ->
    Matrix scalar (Either left right) left
leftProjectionMatrix left right = transposeMatrix (leftInjectionMatrix left right)

-- | Right biproduct projection.
rightProjectionMatrix ::
    (Semiring scalar) =>
    FiniteSet left ->
    FiniteSet right ->
    Matrix scalar (Either left right) right
rightProjectionMatrix left right = transposeMatrix (rightInjectionMatrix left right)

-- | Basis cup, from the singleton object to a tensor square.
cupMatrix ::
    (Semiring scalar) =>
    FiniteSet value ->
    Matrix scalar () (value, value)
cupMatrix object@(UnsafeFiniteSet _) =
    matrixFromFunction unitSet (productSet object object) $ \() (left, right) ->
        if left == right then one else zero

-- | Basis cap, from a tensor square to the singleton object.
capMatrix ::
    (Semiring scalar) =>
    FiniteSet value ->
    Matrix scalar (value, value) ()
capMatrix object = transposeMatrix (cupMatrix object)

-- | Categorical trace over an explicit finite object.
traceMatrix ::
    (CommutativeSemiring scalar) =>
    FiniteSet source ->
    FiniteSet target ->
    FiniteSet traced ->
    Matrix scalar (source, traced) (target, traced) ->
    Either MatrixError (Matrix scalar source target)
traceMatrix source target traced matrix
    | not (sameFiniteSet (productSet source traced) (matrixSource matrix)) =
        Left MatrixTraceObjectMismatch
    | not (sameFiniteSet (productSet target traced) (matrixTarget matrix)) =
        Left MatrixTraceObjectMismatch
    | otherwise =
        Right $ matrixFromFunction source target $ \sourceValue targetValue ->
            foldl'
                plus
                zero
                [ valueOrZero matrix (sourceValue, tracedValue) (targetValue, tracedValue)
                | tracedValue <- finiteSetValues traced
                ]

labelledEntries :: Matrix scalar source target -> [((source, target), scalar)]
labelledEntries (Matrix source target entries) =
    zip
        [ (sourceValue, targetValue)
        | sourceValue <- finiteSetValues source
        , targetValue <- finiteSetValues target
        ]
        entries

valueOrZero ::
    (Semiring scalar) =>
    Matrix scalar source target ->
    source ->
    target ->
    scalar
valueOrZero matrix sourceValue targetValue =
    fromMaybe zero (matrixEntry matrix sourceValue targetValue)

rowsOf :: Int -> Int -> [scalar] -> [[scalar]]
rowsOf rowCount columnCount entries
    | rowCount == 0 = []
    | columnCount == 0 = replicate rowCount []
    | otherwise = take columnCount entries : rowsOf (rowCount - 1) columnCount (drop columnCount entries)

productSet :: FiniteSet left -> FiniteSet right -> FiniteSet (left, right)
productSet (UnsafeFiniteSet left) (UnsafeFiniteSet right) =
    UnsafeFiniteSet [(leftValue, rightValue) | leftValue <- left, rightValue <- right]

sumSet :: FiniteSet left -> FiniteSet right -> FiniteSet (Either left right)
sumSet (UnsafeFiniteSet left) (UnsafeFiniteSet right) =
    UnsafeFiniteSet (map Left left ++ map Right right)

unitSet :: FiniteSet ()
unitSet = UnsafeFiniteSet [()]
