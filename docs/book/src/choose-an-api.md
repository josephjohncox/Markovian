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
| Compare Doğru physical and balance-relaxed finite models | `Markovian.Benchmark.Inventory.Distribution.Dogru.Exact` |
| Solve bounded two-stage fixed-batch execution | `Markovian.Benchmark.Inventory.Serial.FixedBatch.Exact` |
| Check stationary fixed-batch subsystem inequalities | `Markovian.Benchmark.Inventory.Serial.FixedBatch.Newsvendor.Exact` |
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
| Produce a bounded static circuit cost report | `Markovian.Circuit.Interpret.Cost` |
| Check a deterministic structural rewrite exactly | `Markovian.Circuit.Rewrite.Deterministic.Exact` |
| Compile finite deterministic structure | `Markovian.Circuit.Compile.Deterministic` |
| Compose systems through boundaries | `Markovian.Open.StructuredCospan` |
| Execute a validated finite DAG | `Markovian.Open.Acyclic.Circuit.Exact` |
| Define a finite alternating protocol | `Markovian.Game.Arena` and `Markovian.Game.Strategy` |
| Define an owned finite open game | `Markovian.Game.Open.Finite` |
| Enumerate exact pure contextual equilibria | `Markovian.Game.Open.Exact` and `enumeratePureEquilibria` |
| Lower an exact circuit to dense rows | `Markovian.Backend.CPU.Exact` |
| Apply a dense matrix with CUDA | `Markovian.Backend.GPU` |
| Evaluate a checked dense neural network | `Markovian.Backend.Neural.Dense` |
| Construct or inspect a sized structural action mask | `Markovian.Backend.Neural.Mask` |
| Compile exact action availability for a neural head | `Markovian.Backend.Neural.Bridge.ExactSupportMask` |
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

Do not treat a lower declared circuit cost as rewrite evidence. Construct a deterministic candidate and run the exact checker first.

Do not compare finite supports when storage order matters. Use `sameFiniteLayout`.

Do not call a finite value-iteration result an exact optimum. Report its stop reason and residual bounds.

Do not use Q-learning epsilon in the greedy target. Epsilon belongs to its behavior policy.

Do not treat a horizon stop as a terminal state. REINFORCE requires an explicit truncated-boundary bootstrap.

Do not use the CUDA or neural result as the semantic reference. Compare it with exact or checked CPU execution.

Do not use arena histories as open-game strategies. The protocol and open-game modules are separate formalisms.

Do not call a pure contextual equilibrium mixed or subgame perfect. Matching pennies has no result in the implemented pure solution concept, and sequential composition can retain non-credible threats.

Do not mask logits or Q-values by multiplication. Gather available values through a sized structural mask. Use the bridge when the availability comes from a compiled exact MDP.
