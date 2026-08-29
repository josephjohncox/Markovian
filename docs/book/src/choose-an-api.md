# Choose an API

Use this table to select the smallest correct abstraction.

| Task | API |
| --- | --- |
| Construct a checked floating probability | `Markovian.Probability` |
| Construct an exact finite distribution | `Markovian.Probability.Exact` |
| Define one stochastic step | `Markovian.Kernel` or `Markovian.Kernel.Exact` |
| Define an exact MDP | `Markovian.MDP.Exact` |
| Evaluate a short exact policy horizon | `Markovian.Interpreter.Exact` |
| Inspect every exact execution | `exactTraceDistribution` |
| Compile a complete exact MDP | `compileExactMDP` |
| Close a compiled MDP under one policy | `closeCompiledExactPolicy` |
| Evaluate a larger finite policy horizon | `evaluateCompiledExactFinite` |
| Evaluate a discounted policy cycle | `solveCompiledExactPolicy` |
| Run bounded exact value iteration | `solveCompiledExactControl` |
| Run exact deterministic policy iteration | `solveCompiledExactPolicyIteration` |
| Extract deterministic greedy actions | `extractExactGreedyActions` |
| Produce one reproducible trajectory | `Markovian.Interpreter.Sampled` |
| Sample one selected joint MDP outcome | `sampleMDPStep` |
| Evaluate a supplied policy with TD(0) | `Markovian.Learning.TD0` |
| Learn on-policy sampled action values | `Markovian.Learning.Sarsa` |
| Learn expected epsilon-greedy action values | `Markovian.Learning.ExpectedSarsa` |
| Learn with a greedy off-policy target | `Markovian.Learning.QLearning` |
| Run bounded resumable tabular episodes | The corresponding `.Episodic` module |
| Update a POMDP belief | `filterExactBelief` |
| Plan with a finite belief policy | `Markovian.POMDP.Planning.Exact` |
| Build and solve the bounded serial-inventory fixture | `Markovian.Benchmark.Inventory.Serial.Exact` |
| Render its primary-versus-widened report | `Markovian.Benchmark.Inventory.Report` |
| Compose exact weighted linear maps | `Markovian.Category.Matrix` |
| Represent a normalized finite channel | `Markovian.Category.Matrix.Stochastic` |
| Pull an exact finite payoff backward | `pullbackPayoff` in `Markovian.Category.Payoff.Exact` |
| Pair an exact state matrix with a payoff | `pairStatePayoff` in `Markovian.Category.Payoff.Exact` |
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
| Evaluate a checked dense neural network | `Markovian.Backend.Neural.Dense` |
| Compose typed primitive VJPs | `Markovian.Backend.Neural.Reverse` |
| Apply a finite REINFORCE update | `Markovian.Backend.Neural.Reinforce` |
| Apply a one-step actor-critic update | `Markovian.Backend.Neural.ActorCritic` |
| Store explicit replay snapshots | `Markovian.Backend.Neural.Replay` |
| Manage target-network synchronization | `Markovian.Backend.Neural.TargetNetwork` |
| Evaluate or update one DQN batch | `Markovian.Backend.Neural.DQN` |

## Select the control family

Use exact control when you have a complete finite exact model. Value iteration gives a bounded iterate and residual bounds. Policy iteration gives a deterministic policy after exact rational policy solves.

Use tabular learning when you have sampled transitions and a small discrete state-action space. The root update APIs still use an MDP to validate terminal states and action support.

Use the neural package for small function-approximation reference calculations. It has update primitives, but no environment runner or complete trainer.

Read [Control and learning taxonomy](control-learning-taxonomy.md) for the model-based, policy, and representation axes.

## Do not substitute these APIs

Do not use matrix transpose for inference. Use `bayesianInverse` with an explicit prior.

Do not use `bayesianInverse` to evaluate a payoff. Use `pullbackPayoff`; it needs no prior and returns a payoff, not a posterior.

Do not use raw matrix trace for stochastic feedback. Use bounded recursion or a justified fixed-point solver.

Do not use `fanoutCircuit c c` when both consumers must see the same random value. Use `shareCircuit c`.

Do not compare finite supports when storage order matters. Use `sameFiniteLayout`.

Do not call a finite value-iteration result an exact optimum. Report its stop reason and residual bounds.

Do not use Q-learning epsilon in the greedy target. Epsilon belongs to its behavior policy.

Do not treat a horizon stop as a terminal state. REINFORCE requires an explicit truncated-boundary bootstrap.

Do not use the CUDA or neural result as the semantic reference. Compare it with exact or checked CPU execution.
