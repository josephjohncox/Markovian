# Frozen neural-model interrogation

**Status:** Implemented in the repository; unreleased. This document
records the contract and acceptance checks.

## Purpose and boundary

The first target is the checked `DenseNetwork` and `LinearCategoricalPolicy` in
`markovian-neural`. The dense model has `tanh` hidden layers and a linear output head,
but [Dense](../../backends/markovian-neural/src/Markovian/Backend/Neural/Dense.hs) exposes only
outputs and endpoint VJPs. The linear policy has no hidden layers; it already exposes
logits and selected-action log probabilities through
[Policy](../../backends/markovian-neural/src/Markovian/Backend/Neural/Policy.hs).

The first use is a **read-only audit of frozen checkpoints**. The caller supplies the
models, a fixed nonempty bounded set of examples, action masks, and any feature or
action labels.
The library supplies checked internal observations and controlled output comparisons. It
does not assign meanings to units, infer a feature map from an exact state, alter an
optimizer, or claim that a high patch effect identifies a complete mechanism. A unit
address is a zero-based layer/unit coordinate within one topology, not an identity
across independently trained or permuted models.

We choose model-specific probes. A generic reverse-program trace would see the dense
model as [one primitive](../../backends/markovian-neural/src/Markovian/Backend/Neural/Dense.hs), while
the exact stochastic hypergraph represents a different object. Both may warrant separate
designs later.

## Model probes

`traceDense` takes a validated network and finite input. It returns each layer's input,
preactivation, and postactivation in forward order, plus the final output. Hidden layers
are marked `tanh`; the output layer is linear. The implementation must retain
preactivations in the existing checked forward path, preserving its arithmetic order and
the result or error of `denseForward`. It must not create a second evaluator with subtly
different numeric behavior.

`patchDenseHidden` takes one network, recipient input, donor input, one zero-based
**hidden** layer, and a nonempty duplicate-free set of unit indices in that layer. Both
inputs run against the same parameter snapshot. It replaces only the selected recipient
**post-tanh** values with the donor values, then evaluates downstream layers without
changing weights or earlier values. The result records the site, replacement values,
recipient postactivation before the patch, effective postactivation after the patch,
recipient output, donor output, patched output, and checked `patched - recipient`
per-output differences. The effective postactivation need not equal `tanh` of the
recipient preactivation; this is the intervention. A network with no hidden layer has
no valid patch site. Patching
multiple layers at once is deferred because its ordering needs a separate contract.

`inspectLinearPolicy` takes a frozen linear policy, feature vector, and `ActionMask`.
For each global action it reports the checked `Double` products `w[a,i] * x[i]` in
row-major feature order and the resulting logit. These are explicit terms in this
linear model, **not exact arithmetic or causal feature attributions**. It then gathers
admissible logits in the mask's caller-defined order and
uses the existing stable log-softmax semantics to report their log probabilities and
probabilities. Unavailable actions get no probability entry; the report retains global
action indices and mask order. It must neither normalize across unavailable actions nor
reorder ties. Available-action probabilities can underflow to zero in `Double`; a failed
checked softmax calculation remains an error, not an invitation to use a second
normalization rule.

The three entry points return typed `Either` failures for input shape, nonfinite
arithmetic, invalid hidden layer or unit, duplicate or empty patch selection, and mask
mismatch. The audit also rejects empty or over-limit probes and incompatible model
shapes. No entry point partially publishes a report. Reuse existing numeric and mask
validators where possible.

## Frozen-checkpoint audit

A small adapter compares **explicitly supplied** before/after `DQNState` values or
linear policies on one immutable caller-supplied probe set. A positive caller-supplied
probe limit rejects an empty or over-limit set before evaluating any model. Each
probe's feature vector and mask are applied to both snapshots. DQN reports distinguish
online and target networks and include the target successful-update count; they return
all admissible
action values in mask order and their `after - before` differences, not only a chosen
action. Policy reports show masked `after - before` action-probability and
log-probability differences. Each example
keeps its caller-supplied identifier, feature vector, mask, and chosen scalar metric if
one is used. The caller also supplies a snapshot label for each frozen model; the label
and target-update count are provenance, not proof of snapshot identity. An optional
replay entry ID is provenance only: ordinals are scoped to a replay-buffer lineage, and
trainer reports do not themselves preserve a network snapshot or transition payload.

Index-only comparisons require matching input/output dimensions, dense topology for
unit-level comparison, and one ordered mask applied to both snapshots for each probe.
Equal output widths alone do not prove that action names agree; a named-action
comparison needs the bridge's matching `ActionOutputLayout` witnesses. The caller owns semantic
feature names and must pin the same observations for both checkpoints; changing replay
populations is not a before/after model comparison. The existing `DQNBatchEvaluation` is
pre-update evidence, and a later target-network synchronization can also change targets.
The audit therefore reports model behavior at each supplied snapshot without attributing
a change solely to an SGD step. It does not estimate reward, policy quality, or training
improvement without a separately specified environment and evaluation distribution.

Probe results are evidence for a **declared experiment**, not a universal importance
score. Each patch report's vector-valued metric is `patched - recipient`; any scalar
summary is chosen and declared by the caller. The report identifies recipient and donor
inputs and patch site; the caller's snapshot label binds it to a checkpoint in an audit.
A study should
include a same-input donor control and unpatched baseline. Reported localization can
change with the metric and corruption choice ([patching methods](https://arxiv.org/abs/2309.16042)); a
mechanistic claim would additionally need a proposed semantic correspondence and
held-out intervention tests of its faithfulness, as in [causal abstraction](https://arxiv.org/abs/2301.04709).

## Acceptance checks

- `traceDense` output matches `denseForward` for zero, one, and multiple hidden layers,
  including finite-arithmetic failure cases.
- A same-input donor patch reproduces the baseline. When both baseline paths succeed,
  a full-hidden-layer donor patch reproduces the donor output. A selected donor patch
  changes only the chosen layer
  values before downstream evaluation. Repeated runs agree and leave the frozen model
  unchanged.
- Wrong input width, nonfinite values, absent or out-of-range hidden sites, empty or
  duplicate unit selections, and incompatible masks fail with typed errors.
- Each policy action's checked terms reproduce `linearPolicyLogits` using the same
  left-to-right checked sum. Masked probabilities agree with the current categorical
  path in `ActionMask` order, including reordered masks, one admissible action, and
  underflow to zero. An overflowing logit remains an error even if its action is masked
  out.
- A before/after audit rejects mismatched topology or action layout, applies each
  example's mask to both snapshots, distinguishes online from target, and never uses a
  replay ID as checkpoint identity.

Implementation should stay in `markovian-neural` for model probes and
`markovian-neural-bridge` only where replay or exact-action layout types are required.
No new persisted receipt, command-line workflow, GPU tracing, exact-circuit tracing,
transformer interface, or training update belongs in this first slice.

## Implementation sequence

1. Extend the private cache in `Dense.hs` to retain each checked preactivation. Add
   read-only trace types and `traceDense` there, using the existing forward path. Keep
   `denseForward`, both VJPs, and `denseReverseCircuit` behavior unchanged. Use
   opaque public result types with accessors; do not add constructors to the existing
   public `DenseError` sum.
2. Add `patchDenseHidden` beside that cache. Validate recipient and donor vectors,
   the hidden-layer address, and unique unit indices; run both inputs through the
   same network; replace the selected cached postactivations; and resume through the
   remaining `Layer`s. Use a new probe error type for invalid sites and wrapped
   `DenseError`s. Test self-patch identity, full-layer donor equivalence, partial
   patches, zero-hidden rejection, overflow, and unchanged network parameters.
3. Add policy inspection in `Policy.hs`. Refactor its private dot helper so
   `linearPolicyLogits` and the inspection share checked product and sum order. Compute
   all global logits before gathering the caller-ordered mask; use the current stable
   log-softmax semantics for
   probabilities. Test against `linearPolicyLogits` and the categorical API, including
   reordered masks, one active action, underflow, and an overflowing inactive logit.
4. Add one small `markovian-neural` inspection module for read-only frozen comparisons.
   It takes before/after model values, one probe set, snapshot labels, and the probe
   limit; it evaluates every probe under both snapshots and reports online/target
   DQN values or policy probabilities in mask order. The module never calls an update
   function. Numeric indices remain the core result. A thin bridge wrapper for named
   exact actions must accept before/after `ActionOutputLayout` witnesses and require
   `sameActionOutputLayout`; matching support alone is insufficient. Add bridge tests
   for that layout and replay-ID provenance without changing the trainer.
5. Wire the new public module and tests into the neural and bridge Cabal manifests,
   the neural umbrella module, current exposed-module snapshots, and package READMEs.
   Do not mark it released in the capability inventory. Regenerate the existing
   learning output receipt with `python3 scripts/check-learning --write` after source
   changes and review its diff.
   Run the neural unit and integration suites and the bridge suite, the existing
   neural/bridge boundary checks,
   package-manifest and source-archive checks, then the repository CI gates. The
   review criterion is stable output/error behavior and clear provenance, not a
   claimed semantic neuron label or training gain.
