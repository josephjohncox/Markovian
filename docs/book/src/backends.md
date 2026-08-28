# CPU, GPU, and neural boundaries

The semantic core does not depend on tensors, CUDA, autodiff, or a neural framework.

## Dense exact CPU backend

The CPU backend lowers an exact IR or circuit to a row-major rational matrix:

```haskell
dense <- lowerExactCircuit primitives circuit

result <- runDenseExactKernel dense input
```

The finite source and target layouts define the row and column indexes. The backend performs no random draws.

Use this backend as a reference for denotational differential tests and layout inspection.

## CUDA backend

The optional GPU package applies one row-major `Double` matrix to one source distribution:

```haskell
result <- gpuDenseApply rows columns matrix input
```

The result contains the output vector and transfer-inclusive duration.

The implementation includes context setup, host-to-device transfer, kernel execution, device-to-host transfer, and cleanup in its measurement.

CUDA execution is approximate. It does not inherit exact rational circuit laws. The package does not claim support for untested devices or CUDA versions.

## Neural package status

`markovian-neural` is an experimental framework-independent reference package. Its released library depends only on `base`. A separate integration test compares it with the root `Markovian` package. It uses checked `Double` arithmetic and immutable values.

It implements:

- stable softmax and log-softmax;
- analytic categorical Jacobians and selected-action score gradients;
- dense networks with manual vector-Jacobian products;
- masked linear categorical policies and scalar linear value functions;
- an explicit approximation boundary with precision, error, and observation policies;
- executable REINFORCE and one-step actor-critic updates;
- bounded FIFO replay storage;
- hard and Polyak target-network synchronization;
- standard and Double-DQN targets;
- one atomic DQN batch update.

It does not implement a tensor framework, autodiff, device execution, global randomness, an environment runner, or a complete training loop.

## Checked floating arithmetic

Neural constructors reject nonfinite scalars, vectors, and arithmetic results. `FiniteDouble` is available when a durable opaque finite scalar is required. Operations that accept raw `Double` validate inputs and results at their boundaries. `NumericalTolerance` stores explicit absolute and relative tolerances.

Approximate equality uses:

\\[
|x-y|\le \mathrm{atol}+\mathrm{rtol}\max(|x|,|y|).
\\]

A successful result still has ordinary IEEE-754 rounding error. The finite checks do not make the calculation exact.

## Dense networks and manual derivatives

`DenseNetwork` supports zero or more `tanh` hidden layers and one linear output layer:

```haskell
network <-
  mkDenseNetwork
    inputSize
    hiddenSizes
    outputSize
    parameters

output <- denseForward network features
parameterVJP <- denseParameterVJP network features outputCotangent
inputVJP <- denseInputVJP network features outputCotangent
```

For each layer, parameter order is all row-major weights followed by all biases. Layers occur in forward order.

The implementation computes input and parameter vector-Jacobian products manually. Central finite-difference fixtures check every represented derivative with scaled absolute-plus-relative tolerances.

`applySGD` computes every displacement from one pre-update parameter vector. It returns either one complete updated network or an error.

## Stable categorical operations

The package subtracts the largest logit before exponentiation. The softmax Jacobian is:

\\[
\frac{\partial p_i}{\partial z_j}=p_i(\mathbf{1}_{i=j}-p_j).
\\]

For selected available action `a`, the score gradient is computed only over the state's ordered action mask:

\\[
\frac{\partial\log\pi(a)}{\partial z_j}=\mathbf{1}_{a=j}-\pi(j).
\\]

`maximumCategoricalError` compares exact rational masses with approximate masses in max norm. It does not establish a global approximation guarantee.

## REINFORCE with an optional baseline

The implemented REINFORCE update uses a linear categorical policy. It can also use a state-only linear baseline.

For observed rewards and boundary value `b_T`, returns satisfy:

\\[
G_T=b_T,
\qquad
G_t=r_t+\gamma G_{t+1}.
\\]

A terminal boundary supplies the terminal payoff. A truncated boundary must supply an explicit bootstrap value. `TruncatedBoundaryWithoutBootstrap` fails.

```haskell
update <-
  updateReinforce
    config
    policy
    (Just baseline)
    episodeSteps
    (TerminalBoundary terminalPayoff)
```

For the discounted start-return objective, the actor ascent direction is:

\\[
\sum_t \gamma^t(G_t-V(s_t))\nabla_\theta\log\pi_\theta(a_t\mid s_t).
\\]

The baseline ascent direction for negative half-squared error is:

\\[
\sum_t (G_t-V(s_t))\nabla_w V_w(s_t).
\\]

Each policy observation carries a nonempty, duplicate-free ordered action mask. Softmax excludes unavailable outputs, and the score gradient is scattered back into global parameter order with zeros for unavailable actions.

The actor treats the baseline as detached. Actor and baseline gradients use the same pre-update snapshots. The function returns both updated values only after all checks succeed.

The API assumes that the caller collected actions from the supplied policy snapshot. It does not enforce trajectory provenance.

## One-step actor-critic

The actor-critic update also uses a linear categorical policy and linear value function.

Its target is:

\\[
y=
\begin{cases}
r+\gamma g & \text{for a terminal successor},\\
r+\gamma\operatorname{stop}(V(s')) & \text{for a continuing successor}.
\end{cases}
\\]

The TD error is:

\\[
\delta=y-V(s).
\\]

The actor ascends `stop(delta) * log π(a|s)`. The critic descends `0.5 * delta^2` with a detached target.

```haskell
update <-
  updateActorCritic
    config
    policy
    valueFunction
    transition
```

Both gradients use pre-update models. A failure returns neither updated model.

## Replay storage

`ReplayBuffer` is an opaque positive-capacity FIFO buffer:

```haskell
buffer0 <- mkReplayBuffer capacity
let (entryId, buffer1) = appendReplay transition buffer0
batch <- selectReplay WithoutReplacement requestedIds buffer1
```

Entry IDs are monotonic and remain stable until eviction. Appending beyond capacity evicts the oldest entry.

`selectReplay` preserves requested ID order. `WithReplacement` permits repeated IDs. `WithoutReplacement` rejects them. Unknown and evicted IDs fail.

Replay stores immutable validated transition snapshots. The package has no random replay sampler. Callers select IDs explicitly.

## Target networks

A target network stores one dense-network snapshot and a successful-update count. Hard synchronization copies all compatible online parameters.

Polyak synchronization applies:

\\[
\theta^-\leftarrow\tau\theta+(1-\tau)\theta^-,
\qquad 0<\tau\le 1.
\\]

A periodic hard schedule synchronizes after a successful post-update count divisible by its period. A Polyak schedule synchronizes after every successful update.

Failed online updates do not call `afterSuccessfulUpdate`. They do not increment the count or trigger synchronization.

## DQN targets

A `NeuralTransition` stores source features, source action mask, selected action, reward, and one successor snapshot. A terminal snapshot stores a payoff. A continuing snapshot stores successor features and a nonempty ordered action mask.

For standard DQN:

\\[
y=r+\gamma\max_{a'\in M(s')}Q_{\theta^-}(s',a').
\\]

For Double DQN:

\\[
a^*=\underset{a'\in M(s')}{\operatorname{arg\\,max}}
Q^{\mathrm{online}}(s',a'),
\\]

\\[
y=r+\gamma Q_{\theta^-}(s',a^*).
\\]

A terminal target is `r + gamma * g`. Maxima range only over the stored mask. Strict greater-than replacement retains the first mask entry on ties.

## Atomic DQN batch update

Create a DQN state from topology-compatible online and target networks:

```haskell
state <- mkDQNState onlineNetwork targetNetwork
update <- updateDQNBatch config state transitions
```

For a nonempty batch of size `B`, the loss is:

\\[
L(\theta)=\frac{1}{B}\sum_{i=1}^{B}
\frac{1}{2}\left(Q_\theta(s_i,a_i)-y_i\right)^2.
\\]

`evaluateDQNBatch` computes all detached targets, predictions, losses, and parameter gradients from one online snapshot and one target snapshot. `updateDQNBatch` aggregates one mean gradient and attempts one SGD update.

Target synchronization occurs only after the online update succeeds. This function is one update step, not a DQN trainer.

## Approximation and claim boundaries

The neural tests include hand-calculated examples, finite differences, failure atomicity, replay ordering, synchronization timing, and tabular Q-learning differentials for one-hot linear Q networks.

These tests support the implemented finite fixtures. They do not establish convergence, variance reduction, calibration, scalability, accelerator support, or production behavior.

## Further reading

- [Williams: REINFORCE](references.md#williams-reinforce)
- [Sutton and colleagues: policy gradients](references.md#sutton-and-colleagues-policy-gradients)
- [Konda and Tsitsiklis: actor-critic](references.md#konda-and-tsitsiklis-actor-critic)
- [Lin: experience replay](references.md#lin-experience-replay)
- [Mnih and colleagues: DQN](references.md#mnih-and-colleagues-dqn)
- [van Hasselt and colleagues: Double DQN](references.md#van-hasselt-and-colleagues-double-dqn)
- [Higham: floating-point stability](references.md#higham-floating-point-stability)
