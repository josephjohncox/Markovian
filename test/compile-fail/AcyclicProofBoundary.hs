{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module AcyclicProofBoundary where

import Data.Kind (Type)
import Markovian.Circuit
import Markovian.Circuit.Interpret.Exact
import Markovian.Open.Acyclic
import Markovian.Open.Acyclic.Circuit.Exact
import Markovian.Open.Circuit.Exact
import Markovian.Open.StructuredCospan

data Sort = Sort deriving (Eq)
data Primitive (purity :: Purity) (source :: Type) (target :: Type)
data PrimitiveError

rawTopology :: OpenSystem Sort Int Int Int Int Int
rawTopology = undefined

domains :: FiniteValueDomains Sort Bool
domains = undefined

table :: LabelCircuitTable Primitive Sort Int Bool
table = undefined

interpreter :: ExactPrimitiveInterpreter Primitive PrimitiveError
interpreter = undefined

badRawInterpreter = acyclicOpenCircuit rawTopology domains table

badConstructor = UnsafeAcyclicOpenSystem rawTopology undefined

badForgeCycle :: AcyclicOpenSystem Sort Int Int Int Int Int
badForgeCycle = acyclicOpenSystem rawTopology

reversed :: BoundaryReversedOpenCircuit Primitive 'Deterministic () () Sort Int Int Int Int Int
reversed = undefined

badReverseObserver = acyclicOpenCircuitDenotation interpreter reversed

decoration :: Circuit Primitive 'Deterministic () ()
decoration = undefined

badGlobalDecoration =
    acyclicOpenCircuitDenotation interpreter (openCircuit rawTopology decoration)
