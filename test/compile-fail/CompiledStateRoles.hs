module CompiledStateRoles where

import Data.Coerce (coerce)
import Markovian.Compile.Exact (
    CompiledExactMDP,
    CompiledExactMRP,
    CompiledExactMRPState,
    CompiledExactState,
    FiniteStateIndex,
 )

newtype OrdinaryState = OrdinaryState Int
    deriving (Eq)

newtype AllEqualState = AllEqualState Int

instance Eq AllEqualState where
    _ == _ = True

breakFiniteStateIndex :: FiniteStateIndex OrdinaryState -> FiniteStateIndex AllEqualState
breakFiniteStateIndex = coerce

breakCompiledExactState :: CompiledExactState OrdinaryState -> CompiledExactState AllEqualState
breakCompiledExactState = coerce

breakCompiledExactMDP :: CompiledExactMDP OrdinaryState action -> CompiledExactMDP AllEqualState action
breakCompiledExactMDP = coerce

breakCompiledExactMDPAction :: CompiledExactMDP state action -> CompiledExactMDP state otherAction
breakCompiledExactMDPAction = coerce

breakCompiledExactMRPState :: CompiledExactMRPState OrdinaryState -> CompiledExactMRPState AllEqualState
breakCompiledExactMRPState = coerce

breakCompiledExactMRP :: CompiledExactMRP OrdinaryState -> CompiledExactMRP AllEqualState
breakCompiledExactMRP = coerce
