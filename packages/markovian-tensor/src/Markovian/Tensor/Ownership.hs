{-# LANGUAGE RoleAnnotations #-}

{- | Semantic ownership kept separate from physical storage identity.

An owner key states who controls a parameter. A @StorageId@ only identifies a
managed allocation. Equal bytes, shared immutable views, and owner equality do
not imply one another.
-}
module Markovian.Tensor.Ownership (
    TensorOwner,
    tensorOwner,
    ownerKey,
    OwnedTensor,
    ownTensor,
    ownedFiniteTensor,
    ownedTensorOwner,
) where

import Markovian.Tensor.Internal
