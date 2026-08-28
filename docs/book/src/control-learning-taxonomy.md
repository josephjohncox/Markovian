# Control and learning taxonomy

Markovian separates algorithms along three independent axes. The axes describe data use, target policy, and value representation.

## Model-based and model-free targets

A model-based control method reads every transition mass from a finite model. Exact value iteration and exact policy iteration are model-based.

A model-free target reads an observed transition instead of summing over the transition kernel. The tabular TD, REINFORCE, actor-critic, and DQN updates use this target form.

The root learning APIs still receive an `MDP` for validation. They inspect terminal status and available actions. Thus, their targets are model-free in the usual algorithmic sense, but their interfaces are not environment-only.

The episodic tabular runners sample the supplied `MDP`. The neural package has update functions and replay storage, but no environment runner or complete trainer.

## On-policy and off-policy targets

An on-policy target evaluates or improves the policy that generated the observations. An off-policy target can differ from the behavior policy.

| Algorithm | Target relationship | Implemented representation |
| --- | --- | --- |
| TD(0) | On-policy when observations come from the evaluated policy | State-value table |
| SARSA | On-policy when the supplied next action is the next behavior action | Action-value table |
| Expected SARSA | On-policy when its epsilon-greedy expectation is also the behavior distribution | Action-value table |
| Q-learning | Off-policy-capable, using a greedy target | Action-value table |
| REINFORCE | On-policy score-function update | Linear categorical policy and optional linear baseline |
| One-step actor-critic | On-policy actor and TD critic update | Linear categorical policy and linear value function |
| DQN | Off-policy-capable greedy target | Dense neural action-value function |

The pure TD(0), SARSA, and Expected SARSA update functions cannot verify observation provenance. Their on-policy labels apply when callers supply observations and next-action data from the stated behavior policy. The episodic runners construct those relationships. The REINFORCE and actor-critic update functions also do not sample actions; their on-policy meaning assumes that the caller collected actions from the supplied masked policy snapshot.

Q-learning separates behavior from target. Its episodic runner samples epsilon-greedy behavior, but `updateQ` always uses a greedy continuing target.

DQN also uses a greedy target. Replay can reuse older observations, but the package does not provide importance sampling or policy-correction weights.

## Tabular and function-approximation methods

A table stores one independent value per represented state or state-action key. Missing table entries denote zero.

Function approximation shares parameters across inputs. The neural package provides two distinct approximation families:

- REINFORCE and actor-critic use linear policy and value functions.
- DQN uses a checked dense network with `tanh` hidden layers and a linear output head.

The dense network is not wired into the policy-gradient APIs. The package has no recurrent network, convolution, tensor, autodiff, device, or accelerator interface.

## Evaluation and control

Policy evaluation computes the value of a supplied policy. Exact finite-horizon dynamic programming, exact Bellman evaluation, and TD(0) are policy-evaluation methods.

Control selects or improves actions. Exact value iteration, exact policy iteration, Q-learning, SARSA, Expected SARSA, REINFORCE, actor-critic, and DQN are control-oriented updates.

A finite update or bounded run is not a solved optimum. Markovian reports exact residual bounds for discounted exact value iteration. It makes no convergence claim for the sampled or neural algorithms.

## Numeric domains

| Domain | Numeric type | Meaning |
| --- | --- | --- |
| Exact model compilation and control | `Rational` through opaque exact values | Literal finite sums, exact comparisons, and exact linear solves |
| Sampled and tabular learning | Checked `Double` values | Seeded observations and finite arithmetic checks |
| Neural references | Checked `Double` vectors and parameters | Approximate floating computation with explicit shape and finite-value checks |

Exact arithmetic does not make a finite value-iteration iterate equal to the infinite-horizon optimum. The residual and contraction bounds state the remaining error.

Neural finite-difference tests compare analytic derivatives with numerical approximations. They are local checks on finite fixtures, not proofs for all inputs.

## Choose by question

Use exact policy evaluation when you know the policy and complete finite model. Use exact control when you know the complete finite model and need deterministic action choices.

Use tabular learning when states and actions form a small discrete set. Use the pure update functions for algebraic tests. Use the episodic runners for bounded seeded execution.

Use the neural package for small framework-independent reference calculations. It is experimental and does not provide a production training system.
