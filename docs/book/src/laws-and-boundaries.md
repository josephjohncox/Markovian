# Laws, counterexamples, and scope

The framework admits an abstraction only with laws, counterexamples, differential tests, or benchmarks.

## Exact kernel laws

The exact finite kernel domain tests left identity, right identity, and associativity with literal rational equality.

Floating kernels do not claim literal associativity. Different association orders can change rounding.

## Matrix laws

Raw semiring matrices test category, tensor, biproduct, dagger, compact, and trace laws under the required scalar constraints.

A `Semiring` alone does not imply normalization or convex structure. Probability refinements require stronger scalar capabilities.

## Stochastic counterexample

Let a fair coin channel map `()` to `False` and `True` with mass `1/2` each.

The transpose maps each Boolean source to the singleton target with row mass `1/2`. These rows are not normalized.

Therefore, normalized stochastic matrices do not have a general transpose dagger.

## Copy counterexample

Copying one fair coin gives:

\[
P(F,F)=P(T,T)=\frac12.
\]

Executing two fair coins gives:

\[
P(F,F)=P(F,T)=P(T,F)=P(T,T)=\frac14.
\]

Therefore, a stochastic function does not preserve copy. Copy-naturality rewrites require deterministic evidence.

## Bayesian boundary

Bayesian inversion depends on a prior and positive evidence. Two priors can produce different inverse channels for the same likelihood.

Therefore, Bayesian inversion is not a property of the likelihood matrix alone.

## Open-system boundary

Boundary reversal changes the cospan interface direction. It does not reverse internal dynamics.

The acyclic interpreter accepts only uniquely produced DAG topology. It rejects feedback before denotation.

## Supported claims

The current implementation supports:

- Finite discrete carriers.
- Exact rational reference semantics.
- Bounded trace and belief evaluation.
- Finite dynamic programming.
- Discounted contraction Bellman evaluation.
- Finite stochastic circuits.
- Acyclic boundary-functional open networks.
- Explicit approximate backend boundaries.

## Unsupported claims

The implementation does not claim:

- Infinite or continuous carrier semantics.
- Continuous-time Markov process semantics.
- Arbitrary cyclic open-graph evaluation.
- General stochastic trace or feedback.
- Compact closure of normalized stochastic kernels.
- Bayesian inversion as a dagger.
- Automatic differentiation in the semantic core.
- A machine-checked universal coherence theorem.
- Unrestricted Haskell function compilation.
- Arbitrary hypergraph black-boxing.

Treat these items as research tasks with new admission gates.
