-- | Bounded, fallible one-layer exact finite stochastic kernels.
module Markovian.Kernel.Exact (
    ExactKernel,
    ExactKernelError (..),
    exactKernel,
    exactKernelChecked,
    runExactKernel,
    exactDeterministic,
    composeExactKernel,
) where

import Markovian.Probability.Exact (
    ExactBindError (..),
    ExactBindLimits,
    ExactFiniteDist,
    bindExactFiniteDistChecked,
    exactDirac,
 )
import Numeric.Natural (Natural)

{- | An exact kernel with an explicit failure channel. It has no unrestricted
@Category@ or @Arrow@ instance because checked composition requires limits.
-}
newtype ExactKernel input output
    = ExactKernel (input -> Either ExactKernelError (ExactFiniteDist output))

-- | Failures from direct execution or checked composition.
data ExactKernelError
    = ExactKernelRuntimeError !String
    | ExactKernelResultLimitExceeded !Natural
    | ExactKernelWorkLimitExceeded !Natural
    | ExactKernelNumeratorBitsExceeded !Natural
    | ExactKernelDenominatorBitsExceeded !Natural
    deriving (Eq, Show)

-- | Construct an exact kernel from an infallible one-layer function.
exactKernel :: (input -> ExactFiniteDist output) -> ExactKernel input output
exactKernel step = ExactKernel (Right . step)

-- | Construct an exact kernel with an explicit checked failure channel.
exactKernelChecked ::
    (input -> Either ExactKernelError (ExactFiniteDist output)) ->
    ExactKernel input output
exactKernelChecked = ExactKernel

-- | Run one checked exact-kernel layer.
runExactKernel ::
    ExactKernel input output ->
    input ->
    Either ExactKernelError (ExactFiniteDist output)
runExactKernel (ExactKernel step) = step

-- | Lift a deterministic function into a checked exact finite kernel.
exactDeterministic :: (input -> output) -> ExactKernel input output
exactDeterministic function = exactKernel (exactDirac . function)

{- | Compose exact kernels from left to right under one checked-bind budget.
Semantic composition is exact when admitted. Reports and rejection can remain
association-sensitive, so this is not categorical instance evidence.
-}
composeExactKernel ::
    ExactBindLimits ->
    ExactKernel a b ->
    ExactKernel b c ->
    ExactKernel a c
composeExactKernel limits before after =
    exactKernelChecked $ \input -> do
        first <- runExactKernel before input
        case bindExactFiniteDistChecked limits first (runExactKernel after) of
            Left problem -> Left (fromBindError problem)
            Right (result, _) -> Right result

fromBindError :: ExactBindError ExactKernelError -> ExactKernelError
fromBindError problem =
    case problem of
        ExactBindContinuationFailure _ continuationError -> continuationError
        ExactBindResultLimitExceeded observed -> ExactKernelResultLimitExceeded observed
        ExactBindWorkLimitExceeded observed -> ExactKernelWorkLimitExceeded observed
        ExactBindNumeratorBitsExceeded observed -> ExactKernelNumeratorBitsExceeded observed
        ExactBindDenominatorBitsExceeded observed -> ExactKernelDenominatorBitsExceeded observed
