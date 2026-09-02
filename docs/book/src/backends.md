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

The optional GPU package executes only prepared positive-size F64 matrix products and their matrix-product VJPs over checked `markovian-tensor` inputs:

```haskell
prepared <- prepareMatMul limits left right
result <- runPreparedMatMul session
  (PreferCUDA DeterministicFirstDevice FallbackBeforeUserLaunch)
  prepared
```

Preparation bounds dimensions, element products, transfer bytes, scalar work, and user launches before probing a device. CPU execution uses the tensor package's matrix primitive. CUDA admission selects a device by deterministic ordinal, explicit ordinal, or UUID; records capabilities, PTX target, hash, and kernel ABI; loads the committed module; and runs a known-answer self-test.

An admitted executor owns one private context, module, and non-default stream. Its rank-2 scope cannot escape `withCUDAExecutor`. Calls and teardown take the same lock, so close waits for in-flight FFI work before native destruction. A call uses heap-backed host buffers and returns only after stream synchronization and host copy-back. Primary, bounded action-exception, and bounded cleanup diagnostics are retained together.

Launch commitment and fallback permission are separate. Cleanup failure always prohibits fallback but does not fabricate a launch. Matrix VJP carries the first call's launch commitment into every failure from its second call. Configured fallback can occur only before the first user-kernel launch. There is no silent CPU rerun after launch, synchronization, copy-back, nonfinite output, or cleanup failure.

The committed PTX and admitted device profile are bounded to `sm_121` (compute capability 12.1). Host and device admission check every signed kernel index product against `CInt`. A CUDA-enabled build compiles against pinned CUDA 13.0 headers but links only `libdl`. It opens `libcuda.so.1` with `RTLD_NOW | RTLD_LOCAL` and atomically resolves the complete required table, including versioned ABI names, before `cuInit`. Missing libraries, missing symbols, and unsupported devices are explicit pre-launch errors. Required execution returns the error; preferred execution can use the CPU reference only under `FallbackBeforeUserLaunch`.

The executor owns the driver handle and table. It attempts stream, module, and context destruction before `dlclose` and never calls a table entry after unload. The disabled build requires neither headers nor driver libraries. A digest-pinned CUDA 13 compile-only workflow checks the driver-header digest, reproduces the PTX and generated C header, compiles the enabled path with strict warnings, and runs deterministic missing-library and missing-symbol fixtures without a GPU. Protected UUID-bound hardware and four-tool Compute Sanitizer validation remain separate.

Successful current-process admission is not general device-correctness evidence. CUDA results are approximate F64 values and do not inherit exact rational laws. The package does not support arbitrary tensor graphs, generic reverse-program lowering, arbitrary strides, zero-size launches, F32, stochastic nodes, devices outside the pinned profile, cross-device bit equality, or a speed claim.

## Neural package status

`markovian-neural` is an experimental framework-independent reference package. Its library depends on `base` and the backend-independent pure `markovian-reverse` foundation. A separate integration test compares it with the root `Markovian` package. It uses checked `Double` arithmetic and immutable values.

It implements:

- sized structural action masks with checked gather and positive-zero scatter;
- stable softmax and log-softmax;
- analytic categorical Jacobians and selected-action score gradients;
- entropy, cross entropy, KL divergence, mutual information, and analytic logit gradients;
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

## Finite owned reverse programs

`Markovian.Reverse` provides typed `ParametricReverseCircuit` composition for callbacks that return a primal result and captured pullback together. `Markovian.Reverse.Program` adds a finite syntax over a caller-owned primitive GADT. It supports only primitive, identity, composition, tensor, shared-input pairing, and shared-parameter tensor.

Every primitive declares structural parameter ownership and finite layouts for parameter, input, output, and all cotangents. Preparation has explicit program node/depth, primitive, owner, extent, and separate layout/ownership structural node/depth limits. Nodes are charged before descent, including zero-extent and owner-free products. Independent products reject repeated owner keys. A shared-parameter node requires the same complete ownership tree in both branches and adds both parameter cotangents.

A successful forward run returns an opaque typed tape. `StoreCapturedPullback` retains the forward primitive's captured pullback. `RecomputePrimitive` requires a distinct typed owner-supplied recomputation operation; the tape retains immutable parameters, input, and output and checks the recomputed output before applying its pullback. A tape takes no separate program argument. Neither policy schedules checkpoints or estimates bytes.

Exact fixtures check representative composition, tensor, symmetry, interchange, and diagonal laws through explicit pair rearrangements. A nonlinear fixture checks every input and parameter coordinate under both tape policies. The neural package no longer re-exports the reverse modules. Its tests depend on `markovian-reverse` directly. These fixtures test supplied VJPs. They do not establish automatic differentiation of arbitrary Haskell. The extracted interface remains pure; D-067 effect generalization is still required for resource-owning tensor sessions.

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

## Approximate information quantities

`Markovian.Backend.Neural.Information` computes entropy, cross entropy, KL divergence, and finite joint mutual information from stable categorical logits. It also exposes the entropy logit gradient and the fused target-to-prediction cross-entropy gradient.

```haskell
entropy <- entropyFromLogits logits
divergence <- klDivergenceFromLogits sourceLogits targetLogits
gradient <-
  crossEntropyPredictionGradient targetLogits predictionLogits
```

These values use checked `Double` arithmetic because logarithms of rational probabilities are generally irrational. Product-additivity, shift-invariance, decomposition, and finite-difference fixtures are described in [Information theory for finite stochastic models](information-theory.md). The categorical reason VJPs, parameter sharing, and fused gradients compose is developed in [Categorical structure of learning and neural networks](categorical-learning.md).

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

Each policy observation carries a positive complete output width and nonempty, duplicate-free ordered active indices. Softmax gathers available logits before normalization. The score gradient is scattered back into global parameter order with canonical positive `0.0` rows for unavailable actions.

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

## Exact-support bridge

`markovian-neural-bridge` is the only package that imports both the root and neural libraries. It checks a `FiniteActionIndex` against an actual linear-policy or dense-head width. It then compiles each continuing state's exact availability order into a sized `ActionMask`. `SupportMaskLimits` bounds the complete state traversal, cumulative global and local action entries, and a conservative traversal-work charge. Compilation preflights these limits and returns no partial collection after exhaustion.

For global layout `[A,B,C]` and exact local availability `[C,A]`, the bridge produces ordered indices `[2,0]` and flags `[true,false,true]`. It rejects a reordered global layout even when the labelled support is the same. A terminal state produces an explicit terminal result, not an empty all-false mask. Nominal roles prevent `coerce` from relabelling action IDs, finite action indexes, output layouts, or exact support masks.

The bridge performs no rational-to-`Double` conversion and supplies no feature map. The adapter contract is repository-defined. Sutton and Barto §2.8, Mnih and colleagues (2015), and van Hasselt, Guez, and Silver (2016), §4 ground the policy, DQN, and Double-DQN consumers, not this compiler.

## DQN targets

A `NeuralTransition` stores source features, source action mask, selected action, reward, and one successor snapshot. A terminal snapshot stores a payoff. A continuing snapshot stores successor features and a nonempty ordered action mask.

For standard DQN:

\\[
y=r+\gamma\max_{a'\in M(s')}Q_{\theta^-}(s',a').
\\]

For Double DQN:

\\[
a^{\star}=\underset{a'\in M(s')}{\operatorname{arg\\,max}}
Q^{\mathrm{online}}(s',a'),
\\]

\\[
y=r+\gamma Q_{\theta^-}(s',a^{\star}).
\\]

A terminal target is `r + gamma * g`. Continuing maxima gather only the stored available outputs. Strict greater-than replacement retains the first ordered active index on ties. Multiplication by Boolean-as-numeric flags is not masking: it can make an unavailable zero exceed negative available Q-values.

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
- [Shannon: information theory](references.md#shannon-information-theory)
- [Perrone: Markov categories and entropy](references.md#perrone-markov-categories-and-entropy)
- [Cockett and colleagues: reverse derivatives](references.md#cockett-and-colleagues-reverse-derivatives)
- [Griewank and Walther: evaluating derivatives](references.md#griewank-and-walther-evaluating-derivatives)
- [Griewank and Walther: Revolve](references.md#griewank-and-walther-revolve)
- [Higham: floating-point stability](references.md#higham-floating-point-stability)
