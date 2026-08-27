# Choose an API

Use this table to select the smallest correct abstraction.

| Task | API |
| --- | --- |
| Construct a checked floating probability | `Markovian.Probability` |
| Construct an exact finite distribution | `Markovian.Probability.Exact` |
| Define one stochastic step | `Markovian.Kernel` or `Markovian.Kernel.Exact` |
| Define an exact MDP | `Markovian.MDP.Exact` |
| Evaluate a short exact horizon | `Markovian.Interpreter.Exact` |
| Inspect every exact execution | `exactTraceDistribution` |
| Evaluate a larger finite horizon | `evaluateCompiledExactFinite` |
| Evaluate a discounted cycle | `solveCompiledExactPolicy` |
| Produce one reproducible trajectory | `Markovian.Interpreter.Sampled` |
| Learn a bounded tabular policy | `Markovian.Learning.QLearning.Episodic` |
| Update a POMDP belief | `filterExactBelief` |
| Plan with a finite belief policy | `Markovian.POMDP.Planning.Exact` |
| Compose exact weighted linear maps | `Markovian.Category.Matrix` |
| Represent a normalized finite channel | `Markovian.Category.Matrix.Stochastic` |
| Prove that a channel is deterministic | `Markovian.Category.Matrix.Deterministic` |
| Mix channels with exact weights | `Markovian.Category.Convex.Exact` |
| Infer inputs from observations | `Markovian.Bayesian.Exact` |
| Compose prior-indexed channels | `Markovian.Bayesian.Channel.Exact` |
| Preserve shared stochastic results | `shareCircuit` |
| Execute conditionally independent branches | `fanoutCircuit` or tensor |
| Compile finite deterministic structure | `Markovian.Circuit.Compile.Deterministic` |
| Compose systems through boundaries | `Markovian.Open.StructuredCospan` |
| Execute a validated finite DAG | `Markovian.Open.Acyclic.Circuit.Exact` |
| Lower an exact circuit to dense rows | `Markovian.Backend.CPU.Exact` |
| Apply a dense matrix with CUDA | `Markovian.Backend.GPU` |
| Convert logits to a categorical value | `Markovian.Backend.Neural` |

## Do not substitute these APIs

Do not use matrix transpose for inference. Use `bayesianInverse` with an explicit prior.

Do not use raw matrix trace for stochastic feedback. Use a bounded recursion or a justified fixed-point solver.

Do not use `fanoutCircuit c c` when both consumers must see the same random value. Use `shareCircuit c`.

Do not compare finite supports when storage order matters. Use `sameFiniteLayout`.

Do not use the CUDA result as the semantic reference. Compare it with exact or checked CPU execution.
