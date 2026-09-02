{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

{- | Region-bound adapter from the closed host tensor primitives to the
bounded effect reverse interpreter.

The initial fragment contains unary @tanh@ and parameterized pointwise
multiplication. It does not lower arbitrary reverse programs, callbacks, CUDA
kernels, or generic tensor operations.
-}
module Markovian.Tensor.Reverse.Program (
    TensorReverseError (..),
    TensorReverseExecutor,
    withTensorReverseExecutor,
    tensorReverseFromList,
    TensorZero,
    tensorReverseZero,
    TensorReversePrimitive,
    tensorTanhProgram,
    tensorMultiplyProgram,
    resolveTensorReversePrimitive,
) where

import Control.Exception (AsyncException, Exception, SomeException, displayException, fromException, throwIO, try)
import Markovian.Reverse (
    CotangentEqualityMode (ApproximateCotangentEquality, ExactCotangentEquality),
    FiniteLayout,
    declaredCotangentSpace,
    finiteLayout,
    unitFiniteLayout,
 )
import Markovian.Reverse.Program
import Markovian.Reverse.Program.Effect
import Markovian.Tensor
import Markovian.Tensor.Primitive (add)
import Markovian.Tensor.Reverse (applyBinaryTape, applyUnaryTape, multiplyWithTape, tanhWithTape)

-- | Failures admitted by the bounded host adapter.
data TensorReverseError
    = TensorReverseTensorFailure !TensorError
    | TensorReverseActionException !String
    | TensorReverseActionAndCleanupFailure !TensorReverseError ![String]
    | TensorReverseExceptionAndCleanupFailure !String ![String]
    deriving (Eq, Show)

-- Private control exception used only to carry a typed action-level 'Left'
-- through the tensor session's exception-safe cleanup boundary.
newtype TensorReverseAbort = TensorReverseAbort TensorReverseError
    deriving (Show)

instance Exception TensorReverseAbort

-- | Opaque capability tying every callback and tape to one tensor session.
newtype TensorReverseExecutor region = TensorReverseExecutor (TensorSession region)

type role TensorReverseExecutor nominal

{- | Acquire, use, and close one tensor reverse executor. Tensors, tapes, storage
IDs, and the executor cannot escape the rank-2 region. Session cleanup runs
before a synchronous action exception is reported.
-}
withTensorReverseExecutor ::
    SessionLimits ->
    (forall region. TensorReverseExecutor region -> IO (Either TensorReverseError value)) ->
    IO (Either TensorReverseError value)
withTensorReverseExecutor limits action = do
    outcome <- try @SomeException $ withTensorSession limits $ \session -> do
        result <- action (TensorReverseExecutor session)
        case result of
            Left problem -> throwIO (TensorReverseAbort problem)
            Right value -> pure (Right value)
    case outcome of
        Left problem ->
            case fromException problem :: Maybe TensorSessionException of
                Just (TensorSessionException primary diagnostics) ->
                    case fromException primary :: Maybe TensorReverseAbort of
                        Just (TensorReverseAbort actionProblem) ->
                            pure (Left (TensorReverseActionAndCleanupFailure actionProblem diagnostics))
                        Nothing -> case fromException primary :: Maybe AsyncException of
                            Just _ -> throwIO problem
                            Nothing -> pure (Left (TensorReverseExceptionAndCleanupFailure (displayException primary) diagnostics))
                Nothing -> case fromException problem :: Maybe TensorReverseAbort of
                    Just (TensorReverseAbort actionProblem) -> pure (Left actionProblem)
                    Nothing -> case fromException problem :: Maybe AsyncException of
                        Just asynchronous -> throwIO asynchronous
                        Nothing -> pure (Left (TensorReverseActionException (displayException problem)))
        Right (Left problem) -> pure (Left (TensorReverseTensorFailure problem))
        Right (Right result) -> pure (Right result)

-- | Allocate one finite input inside the executor's region.
tensorReverseFromList :: TensorReverseExecutor region -> SShape shape -> [Double] -> IO (Either TensorReverseError (FiniteTensor region 'F64 shape))
tensorReverseFromList (TensorReverseExecutor session) shape values =
    fmap (fmap fst . mapTensorError) (finiteTensorFromList session shape values)

-- | Opaque allocated additive identity for one static tensor shape.
newtype TensorZero region shape = TensorZero (FiniteTensor region 'F64 shape)

type role TensorZero nominal nominal

-- | Allocate the tensor additive identity inside the executor.
tensorReverseZero :: TensorReverseExecutor region -> SShape shape -> IO (Either TensorReverseError (TensorZero region shape))
tensorReverseZero executor shape = fmap (fmap TensorZero) (tensorReverseFromList executor shape (replicate (fromIntegral (shapeElements shape)) 0))

{- | Closed tensor primitive symbols. Constructors are hidden so a resolver
never receives an arbitrary Haskell callback.
-}
data TensorReversePrimitive region parameter parameterCotangent input inputCotangent output outputCotangent where
    TensorTanh ::
        SShape shape ->
        TensorZero region shape ->
        TensorReversePrimitive region () () (FiniteTensor region 'F64 shape) (FiniteTensor region 'F64 shape) (FiniteTensor region 'F64 shape) (FiniteTensor region 'F64 shape)
    TensorMultiply ::
        String ->
        SShape shape ->
        TensorZero region shape ->
        TensorReversePrimitive region (FiniteTensor region 'F64 shape) (FiniteTensor region 'F64 shape) (FiniteTensor region 'F64 shape) (FiniteTensor region 'F64 shape) (FiniteTensor region 'F64 shape) (FiniteTensor region 'F64 shape)

-- | Closed @tanh@ syntax.
tensorTanhProgram :: SShape shape -> TensorZero region shape -> ReverseProgram (TensorReversePrimitive region) TensorReverseError Double () () (FiniteTensor region 'F64 shape) (FiniteTensor region 'F64 shape) (FiniteTensor region 'F64 shape) (FiniteTensor region 'F64 shape)
tensorTanhProgram shape = primitiveProgram . TensorTanh shape

{- | Closed pointwise multiplication syntax. The first operand is the named
parameter and the second operand is the program input.
-}
tensorMultiplyProgram :: String -> SShape shape -> TensorZero region shape -> ReverseProgram (TensorReversePrimitive region) TensorReverseError Double (FiniteTensor region 'F64 shape) (FiniteTensor region 'F64 shape) (FiniteTensor region 'F64 shape) (FiniteTensor region 'F64 shape) (FiniteTensor region 'F64 shape) (FiniteTensor region 'F64 shape)
tensorMultiplyProgram owner shape = primitiveProgram . TensorMultiply owner shape

-- | Resolve the closed symbols against one region-bound executor.
resolveTensorReversePrimitive :: TensorReverseExecutor region -> EffectReversePrimitiveResolver IO (TensorReversePrimitive region) TensorReverseError Double
resolveTensorReversePrimitive executor@(TensorReverseExecutor session) symbol = case symbol of
    TensorTanh shape zero -> do
        let primal = tensorPrimal shape
            cotangent = tensorCotangent executor shape zero
        effectOwnedReversePrimitive
            "tensor/tanh"
            "host-f64-v1"
            noParameterOwnership
            unitPrimal
            unitCotangent
            primal
            cotangent
            primal
            cotangent
            ( \() input -> do
                result <- tanhWithTape session input
                pure $ case result of
                    Left problem -> Left (TensorReverseTensorFailure problem)
                    Right (output, tape, _) -> Right (effectReverseEvaluation output (fmap (fmap (\(gradient, _) -> ((), gradient)) . mapTensorError) . applyUnaryTape session tape))
            )
    TensorMultiply owner shape zero -> do
        layout <- maybe (Left EmptyParameterOwner) Right (tensorLayoutWitness shape)
        ownership <- parameterOwner owner layout
        let primal = tensorPrimal shape
            cotangent = tensorCotangent executor shape zero
        effectOwnedReversePrimitive
            "tensor/multiply"
            "host-f64-v1"
            ownership
            primal
            cotangent
            primal
            cotangent
            primal
            cotangent
            ( \parameter input -> do
                result <- multiplyWithTape session parameter input
                pure $ case result of
                    Left problem -> Left (TensorReverseTensorFailure problem)
                    Right (output, tape, _) -> Right (effectReverseEvaluation output (fmap (fmap fst . mapTensorError) . applyBinaryTape session tape))
            )

mapTensorError :: Either TensorError value -> Either TensorReverseError value
mapTensorError = either (Left . TensorReverseTensorFailure) Right

unitPrimal :: FinitePrimalSpace TensorReverseError ()
unitPrimal = finitePrimalSpace unitFiniteLayout (const (Right ())) (==) ExactCotangentEquality

unitCotangent :: EffectCotangentSpace IO TensorReverseError Double ()
unitCotangent = effectCotangentSpace underlying (\() () -> pure (Right ()))
  where
    underlying = case declaredCotangentSpace "tensor/unit" unitFiniteLayout (const (Right ())) () (\() () -> Right ()) (\_ () -> Right ()) (==) ExactCotangentEquality of
        Nothing -> error "static unit cotangent declaration was rejected"
        Just space -> space

tensorPrimal :: SShape shape -> FinitePrimalSpace TensorReverseError (FiniteTensor region 'F64 shape)
tensorPrimal shape =
    finitePrimalSpace
        (requiredLayout shape)
        (const (Right ()))
        (\left right -> sameStorage (hostTensor left) (hostTensor right))
        (ApproximateCotangentEquality "host-f64 payload semantics; recomputation unsupported")

tensorCotangent :: TensorReverseExecutor region -> SShape shape -> TensorZero region shape -> EffectCotangentSpace IO TensorReverseError Double (FiniteTensor region 'F64 shape)
tensorCotangent (TensorReverseExecutor session) shape (TensorZero zero) = effectCotangentSpace underlying addM
  where
    underlying = case declaredCotangentSpace owner layout (const (Right ())) zero (\_ _ -> error "pure tensor cotangent addition was evaluated") (\_ _ -> error "pure tensor cotangent scaling was evaluated") equivalent (ApproximateCotangentEquality "host-f64") of
        Nothing -> error "static tensor cotangent declaration was rejected"
        Just space -> space
    layout = requiredLayout shape
    owner = "tensor/f64/" ++ show (shapeDimensions shape)
    equivalent left right = sameStorage (hostTensor left) (hostTensor right)
    addM left right = fmap mapTensorError (addTensorCotangents session left right)

addTensorCotangents :: TensorSession region -> FiniteTensor region 'F64 shape -> FiniteTensor region 'F64 shape -> IO (Either TensorError (FiniteTensor region 'F64 shape))
addTensorCotangents session left right = fmap (fmap fst) (add session left right)

tensorLayoutWitness :: SShape shape -> Maybe FiniteLayout
tensorLayoutWitness shape = finiteLayout ("tensor/f64/" ++ show (shapeDimensions shape)) (shapeElements shape)

requiredLayout :: SShape shape -> FiniteLayout
requiredLayout shape = case tensorLayoutWitness shape of
    Nothing -> error "static tensor layout declaration was rejected"
    Just layout -> layout
