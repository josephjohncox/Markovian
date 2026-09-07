# Three learning routes

Start with [installation](installation.md) and the [notation guide](orientation.md).
All commands run at the repository root. The [snippet convention](book-workflow.md#snippet-classification)
distinguishes runnable programs, compiled fragments and conceptual sketches.

## Route 1: MDP practice

**Prerequisites:** Haskell algebraic data types, `Either`, finite probability,
and a discounted sum. No category theory is required.

**Outcomes:** build a joint reward/successor model; explain terminal-before-horizon
timing; choose trace enumeration, finite dynamic programming or strict-discount
control without confusing evaluation with learning.

1. Run the [first MDP](first-mdp.md).
2. Read [model semantics](model-semantics.md), then [exact evaluation](exact-evaluation.md).
3. Predict and check [laboratory C and D](law-laboratory.md#c-reward-and-successor-correlation).
4. Compare [sampling and tabular learning](sampling-learning.md) with the exact reference.

**Checkpoint:** with immediate reward 2, terminal payoff 7 and discount 1/2,
explain why one transition gives 11/2 but zero transitions from a continuing
state give zero. The checked solution is laboratory D.

**Next:** [inventory control](inventory-control.md), then [POMDPs](pomdp.md) when
observations rather than complete states drive decisions. Do not assume a
fixed-policy evaluation theorem proves policy optimization or convergence.

## Route 2: Probability and composition

**Prerequisites:** finite sums, conditional probability and Haskell functions.
Use the [category primer](category-primer.md) for products and composition.

**Outcomes:** read source-by-target matrices; distinguish execution from sharing;
compute prior-indexed inversion only on supported evidence; recognize the
proof boundary between deterministic and stochastic copy laws.

1. Read [probability and kernels](probability-kernels.md) and [matrices](matrices.md).
2. Read [matrix refinements](matrix-refinements.md) and [Bayesian inference](bayesian.md).
3. Predict and check [laboratory A and B](law-laboratory.md#a-shared-randomness-and-determinism).
4. Continue to [circuits](circuits.md) and [laws and boundaries](laws-and-boundaries.md).

**Checkpoint:** explain why a matrix transpose whose first row sums to 3/2
cannot be a Bayesian inverse. Laboratory B computes the actual posterior.

**Next:** [categorical probability](categorical-probability.md), then
[restricted continuous probability](continuous-probability.md). The latter
represents a bounded fragment, not arbitrary measures or integration.

## Route 3: Differentiation and backends

**Prerequisites:** matrix multiplication, directional derivatives, the chain rule,
and the distinction between a logical array and its storage. Review the
[algebra primer](algebra-primer.md) if exact equality is unfamiliar.

**Outcomes:** distinguish payoff pullback, Bayesian inverse and VJP; identify
an F64 operation order; name which logical coordinates a cotangent belongs to;
avoid inferring GPU or general autodiff support from a host fixture.

1. Read [orientation](orientation.md#three-different-reverse-operations) and
   [polarity and games](polarity-and-games.md).
2. Read [bounded autodiff lowering](autodiff-lowering.md) and [tensor runtime](tensor-runtime.md).
3. Predict and check [laboratory E and F](law-laboratory.md#e-exact-equality-and-floating-reassociation).
4. Read [backend boundaries](backends.md) and the [checked capabilities](capabilities.md).

**Checkpoint:** a transpose view shares storage with its base, yet its logical
cotangent order is different. Laboratory F checks both the primitive-on-view
VJP and an explicitly composed transpose pullback.

**Next:** [categorical learning](categorical-learning.md), then the
[evidence records](evidence.md). These host experiments do not execute CUDA,
prove arbitrary view differentiation, or certify training performance.
