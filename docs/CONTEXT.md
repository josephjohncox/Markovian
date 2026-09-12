# Repository context

Markovian separates exact finite semantics from numerical and hardware
execution. The root library depends only on `base`; optional packages own
sampling, learning, continuous kernels, differentiation, tensors, and CUDA.
See the [package map](../README.md#present-api-surfaces) and
[architecture](ARCHITECTURE.md) for their interfaces and dependency boundaries.

## Current state

The published `v2026.9.3.0` release is pinned in
[`release/published-releases.json`](../release/published-releases.json).
The development tree still uses those package versions. A version number alone
does not establish whether a development API was released.

- D-053 through D-076 are Accepted for their recorded release scopes.
- D-077 through D-081 are Accepted and unreleased. They cover GPU evidence
  policy, strict-discount affine feedback, joint-affine substitution,
  cumulative quotation compilation, and host-F64 affine views.
- EL-03, EL-04, and EL-05 are implemented and unreleased; their decisions remain
  Proposed.
- D-083's CE/CCE solvers are implemented and unreleased. D-083 remains Proposed
  pending accounting instrumentation, failure controls, and verification.
- D-082 CUDA graphs, D-084's reference trainer, and D-085's concrete trace cache
  remain Proposed and unimplemented. EL-06 supplies a resource-admission model
  for D-085, not the cache implementation.

The [capability inventory](book/src/capabilities.md) checks selected status and
module claims. [TODO.md](../TODO.md#open-work) tracks remaining work;
[DECISIONS.md](DECISIONS.md) records acceptance and contract changes.

Development uses GHC 9.14.1 and Cabal 3.18.1.0. Versions are pinned in
[`toolchain.env`](../toolchain.env); [installation](book/src/installation.md)
explains project-local tooling and HLS. GHC 9.8.4 builds ancillary tools only.

## Interpretation limits

The affine-view [implementation record](evidence/D081-AFFINE-IMPLEMENTATION.md)
explains its logical resource model and callback lifetime requirements.
Logical work and storage accounting do not measure GHC allocation or guarantee
physical reclamation.

The CUDA executor supports one declared F64 matrix/VJP profile. Hardware
claims require [deployment-bound evidence](WORKFLOWS.md#gpu-deployment-evidence).
Historical CUDA summaries cannot establish correctness for a later revision.

The continuous packages support their declared algebraic and event fragments;
point conditioning, arbitrary measurable callbacks, and general disintegration
remain outside their contracts. General cyclic feedback, arbitrary-Haskell
autodiff, unrestricted equilibrium solving, and production training are also
outside the current API.

## Semantic vocabulary

- **Finite distribution**: a validated, nonempty finite support with positive exposed mass and normalized total mass.
- **Stochastic kernel**: a function from one input to a finite distribution over outputs.
- **Action ID**: the stable identity of an agent choice.
- **Transition outcome**: one transition reward paired with its successor state.
- **Policy**: a stochastic kernel from state to available action ID.
- **Objective**: the explicit horizon, discount, and return rule supplied to an interpreter.
- **Interpreter**: code that evaluates, samples, solves, learns, or lowers a model.
- **Bellman operator**: the value transformation for one objective step.
- **Model syntax**: an explicit recursive program representation, if one is introduced later. It is not the state graph.

## Non-negotiable invariants

| ID | Invariant | Enforcement |
| --- | --- | --- |
| INV-ACTION | Action IDs and stochastic outcomes are different types. | Module boundaries and opaque constructors |
| INV-DIST | Finite distributions reject empty, negative, non-finite, and zero-total input. | Smart constructors |
| INV-REWARD | Rewards and terminal payoffs are finite. | Smart constructors |
| INV-AVAILABLE | Continuing MDP states have nonempty unique action IDs. | Structured model errors |
| INV-POLICY | Policy support is unique and contained in available actions. | Validation at closure and evaluation |
| INV-OBJECTIVE | Evaluation receives an explicit named objective. | Required argument |
| INV-TERMINAL | No interpreter requests policy or transition data at a terminal state. | Control flow and sentinels in tests |
| INV-HORIZON | Bounded evaluation decreases the transition horizon on every recursive call. | `Natural` horizon and self-loop tests |
| INV-RNG | Reproducible stochastic execution receives and returns explicit generator state. | Sampled interpreters and episodic runners |
| INV-CONTROL | Exact control compiles every model action and preserves each joint reward-successor distribution. | Policy-free compiled MDP and exact-control tests |
| INV-INVENTORY | Exact inventory values apply only to the stated finite or conditional bounded-demand model; reachable successors are not clamped, truncation mass is not a value-error bound, and unrelated or non-widened solutions cannot produce stable evidence. Fixed-batch finite-horizon and stationary outputs remain separate. | Opaque fixture and solution, support closure, checked widening witness, separate newsvendor module, and report tests |
| INV-LEARNING | Terminal updates use `r + gamma * g` without dummy actions or terminal policy queries. | Pure tabular and neural update tests |
| INV-MASK | Neural masks have a checked complete width and ordered nonempty active indices; exact compilation checks global layout, preserves availability order, and keeps terminals separate. | Opaque mask/layout constructors, bridge tests, gather-before-consumer implementation |
| INV-CORE | The semantic core has no tensor, GPU, autodiff, neural, or sampling-framework dependency. | Cabal dependency check |
| INV-TENSOR | Tensor parameter ownership, cotangent algebra, and physical storage identity remain separate; numerical primitives accept finite F64 values only. | Opaque owners/storage, finite refinement, nominal roles, tensor tests |
| INV-DEVICE | A CUDA executor owns one private context, module, and stream; calls synchronize before copy-back and retain cleanup failures. | Opaque serialized executor, safe FFI transaction, structured status, enabled tests |
| INV-FALLBACK | Automatic CPU fallback can occur only when configured and before the first user-kernel launch. | Committed-launch status and dispatch tests |
| INV-REVERSE | Independent parameters form explicit pairs; cotangent witnesses provide zero, addition, scalar action, and equality; primitive pullbacks are additive and homogeneous; reverse diagonals use witness addition. | Opaque reverse interpreter, exact module/VJP laws, and nonlinear finite differences |
| INV-COPY | Copy targets the full tensor square and assigns mass only to diagonal pairs. | Opaque IR constructors and exact law tests |
| INV-MATRIX | Matrix semantics compare labelled entries and supports, not storage layout or exact-distribution representation. | Opaque matrices and `matrixEquivalent` laws |
| INV-NORMALIZED | Stochastic matrix rows sum exactly to one; transpose and trace are raw-matrix operations only. | Opaque normalized refinement and counterexample test |
| INV-PAYOFF | Exact payoffs are total on their finite object; pullback is conditional expectation and state pairing checks the represented singleton state source and common object. | Opaque payoff tables, structured mismatch errors, and exact pairing fixtures |
| INV-DETERMINISTIC | Copy-naturality reasoning requires proof-carrying deterministic provenance. | Opaque deterministic refinement and total forgetful embedding |
| INV-BAYES | Bayesian inversion is prior-indexed and defined only from positive output support to positive input support. | Opaque inverse, structured zero evidence, and exact joint laws |
| INV-CIRCUIT-PURITY | Copy-naturality rewrites require deterministic construction provenance. | Purity index, one-way weakening, and compile-fail checks |
| INV-CIRCUIT-SHARING | One stochastic execution followed by copy differs from duplicated execution. | Distinct share and fanout nodes with differential laws |
| INV-CIRCUIT-COST | Cost reports are bounded static accounting with caller-owned primitive charges; layout width is not runtime or heap liveness. | Raw-node-bounded fold, `Natural` limits, deterministic reports |
| INV-CIRCUIT-REWRITE | A deterministic candidate becomes checked evidence only after bounded exact endpoint-layout and row-major matrix checks. | Opaque candidates and checked witnesses, exact differential tests, compile-fail purity boundary |
| INV-OPEN-REVERSAL | Boundary reversal swaps cospan legs and does not reverse dynamics or circuit state parameters. | Same-oriented state types and a separate view with no reverse-denotation observer |
| INV-OPEN-CELLS | Higher cells preserve types, labels, ordered incidence, and both boundary squares. | Opaque validated `OpenSystemCell` |
| INV-OPEN-DAG | Every interpreted apex vertex has exactly one producer and represented edges are acyclic. | Opaque `AcyclicOpenSystem`, stable cycle diagnostics, and compile-fail checks |
| INV-OPEN-EXECUTION | One edge occurrence executes once; multiple consumers read its stored value, while separate occurrences execute independently. | Named assignments, edge-local circuit interpretation, and exact sharing fixtures |

## Before changing semantics

Read the affected contract and its tests. Keep the invariants above, explicit
resource limits, error order, and equality definitions in view. Follow
[WORKFLOWS.md](WORKFLOWS.md) for validation and documentation changes. Record
open work in `TODO.md` and review results in the PR.
