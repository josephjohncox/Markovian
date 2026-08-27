{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module AcyclicPurity where

import Data.Kind (Type)
import Markovian.Circuit
import Markovian.Open.Acyclic.Circuit.Exact

data Primitive (purity :: Purity) (source :: Type) (target :: Type)

data Sort
data Input
data Output
data Vertex
data Edge
data Label
data Value

badStrengthenAggregate ::
    AcyclicOpenCircuit Primitive 'Stochastic Sort Input Output Vertex Edge Label Value ->
    AcyclicOpenCircuit Primitive 'Deterministic Sort Input Output Vertex Edge Label Value
badStrengthenAggregate = id
