{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module GraphBoundary where

import Data.Coerce (coerce)
import Data.Kind (Type)
import Markovian.Backend.GPU.Graph
import Markovian.Tensor
import Markovian.Tensor.Affine (AffineMap)

badRefConstructor = Here
badGraphConstructor = Input
badPreparedConstructor = PreparedGraph
badVJPConstructor = PreparedGraphVJP
badResultConstructor = GraphResult
badGradientConstructor = GraphGradient

badEnvironment :: MatrixRef '[ '(2, 2)] 2 2 -> MatrixRef '[ '(3, 3)] 2 2
badEnvironment = coerce

badGraphRegion :: Graph '[] (region :: Type) 2 2 -> Graph '[] (other :: Type) 2 2
badGraphRegion = coerce

badPreparedShape :: PreparedGraph region 2 2 -> PreparedGraph region 2 3
badPreparedShape = coerce

badResultShape :: GraphResult 2 2 -> GraphResult 2 3
badResultShape = coerce

badFreeVariable :: Graph '[] region 2 2
badFreeVariable = matrixRef matrixHere

badMultiplyShape :: Graph '[] region 2 3 -> Graph '[] region 2 2 -> Graph '[] region 2 2
badMultiplyShape = matrixMultiply

badAffineShape :: AffineMap map '[3, 2] '[2, 2] -> Graph '[] region 2 3 -> Graph '[] region 2 2
badAffineShape = matrixView

badSessionEscape :: SessionLimits -> GraphLimits -> IO (Either TensorError (Either GraphError (PreparedGraph region 2 2)))
badSessionEscape sessionBudget graphBudget = withTensorSession sessionBudget $ \session -> do
    tensor <- finiteTensorFromList session knownShape [1, 2, 3, 4]
    pure (fmap (\(value, _) -> prepareGraph graphBudget (matrixInput value)) tensor)
