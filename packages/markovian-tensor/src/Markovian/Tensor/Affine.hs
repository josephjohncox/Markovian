{- | Checked signed affine views and fresh original-base pullbacks.

A pure map contains no tensor, owner, region, pointer or storage identity. It
may be compiled before allocating a base, and deliberately rebound to different
explicitly supplied compatible contiguous bases. Compilation constructs bounded
Haskell metadata, not tensor payload. Maps from other planning budgets remain
usable; runtime charges current work without importing their planning history.

Equal-shaped finite cotangent seeds of independent storage, values or semantic
origin are intentionally accepted. Seeds certify neither base nor map identity.
The binding retains the actual supplied original base, semantic owner and map;
pullback returns that owner with a fresh logical-base-shaped tensor, not a
capacity-shaped or view-shaped result. Different explicit semantic owners may
share storage. Reachability of a binding or ForeignPtr is not a lifetime lease.

All region-dependent observation, refinement, primitives and allocation must
execute and complete inside the session callback. The caller must join or
cancel-and-join dependent children before every exit, including Left, synchronous
exceptions and asynchronous interruption; the runner does not join them. Never
later execute escaped actions or observe existentially retained tensors. Ordinary
copied data whose reads completed inside the callback may be returned. Nominal
indices do not enforce temporal confinement or observer/close synchronization.

Affine policy is disabled by default. Immutable planning budgets account only
for explicitly threaded successful paths; branching, retries and discarded
attempts are not a global quota. Runtime usage is serialized under the session
transaction and is separate from planning history and the old payload/scalar
account. Reservations bound the specified logical engine, not CPU instructions,
GHC heap allocation or physical reclamation. Allocator/finalizer execution,
external callbacks, OS/GC allocation, lock waiting, arbitrary caller thunks and
continuations, owner String contents and exception String rendering are excluded.
Cleanup callbacks may fail or be retried after asynchronous interruption; their
execution and diagnostic strings have no finite engine-cost theorem.
-}
module Markovian.Tensor.Affine (
    AffineLimits,
    AffineBudget,
    AffineMap,
    OwnedAffineView,
    AffineLimit (..),
    AffineInput (..),
    AffineProblem (..),
    AffineUsage (..),
    AffineCharge (..),
    AffineMapReport (..),
    AffineOperationReport (..),
    affineLimits,
    tensorSessionLimitsWithAffine,
    affineBudget,
    affineBudgetUsage,
    withAffineMap,
    permuteAffineMap,
    reverseAffineMap,
    sliceAffineMap,
    bindAffineView,
    affineViewTensor,
    affineViewBase,
    affineViewMap,
    pullbackAffineView,
) where

import Markovian.Tensor.Internal
