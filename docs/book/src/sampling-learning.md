# Sampling and tabular learning

The sampled and tabular modules use validated `Double` values. They remain separate from exact `Rational` semantics.

## Explicit generators and joint steps

A sampled evaluation consumes one generator and returns the next generator:

```haskell
sample <- sampleReturn objective model policy generator

sampledReturn sample
sampledTrace sample
sampledGenerator sample
```

Use the returned generator for the next draw. Reusing the input generator repeats the same random stream.

`sampleMDPStep` is the shared one-step operation:

```haskell
(traceStep, nextGenerator) <-
  sampleMDPStep model state selectedAction generator
```

It validates the selected action through `stepMDP`. It then samples the joint reward-successor outcome. It never samples reward and successor independently.

## Sample timing

The sampled evaluators use the same reward and terminal timing as the exact evaluators.

1. Inspect the current state.
2. Stop and collect its payoff if it is terminal.
3. Stop without a payoff if the horizon is zero.
4. Select an available action.
5. Sample one joint reward and successor outcome.
6. Continue with one fewer transition.

A transition into a terminal state contributes:

\\[
r+\gamma g.
\\]

The runner inspects terminal status before the step limit. Therefore, a terminal successor reached by the last permitted transition still contributes its payoff.

A horizon stop at a continuing state contributes no implicit terminal payoff or bootstrap value.

## Shared tabular values

`Markovian.Learning.Tabular` defines the common values and schedules:

- `QTable state action` stores action values.
- `VTable state` stores state values.
- Missing keys denote zero.
- Explicit table constructors reject duplicate keys and nonfinite values.
- Learning rates lie in `(0,1]`.
- Exploration rates lie in `[0,1]`.
- The implemented schedules are constant.

Every pure update has this form:

\\[
x_{t+1}=x_t+\alpha(y_t-x_t).
\\]

The algorithms differ in the target `y_t`.

## TD(0)

TD(0) uses a state-value target. The episodic runner evaluates its supplied behavior policy; the pure `updateTD0` function assumes the observation came from that policy but does not receive or verify it. For a continuing successor:

\\[
y_t=r_{t+1}+\gamma V(S_{t+1}).
\\]

For a terminal successor with payoff `g`:

\\[
y_t=r_{t+1}+\gamma g.
\\]

Apply one pure update:

```haskell
updated <-
  updateTD0 rate discount model observedTransition valueTable
```

The observed action identifies what the behavior policy did. The update does not maximize over successor actions.

## SARSA

SARSA uses the next action selected by the behavior policy:

\\[
y_t=r_{t+1}+\gamma Q(S_{t+1},A_{t+1}).
\\]

The pure update requires an explicit bootstrap:

```haskell
updated <-
  updateSarsa
    rate
    discount
    model
    observedTransition
    (SarsaNextAction nextAction)
    table
```

Use `SarsaTerminal` for a terminal successor. No dummy next action is required.

The episodic runner selects `A_(t+1)` before it updates `(S_t,A_t)`. It carries exactly that action into the next sampled step.

## Expected SARSA

Expected SARSA uses an expectation under the canonical epsilon-greedy behavior distribution:

\\[
y_t=r_{t+1}
+\gamma\sum_a\pi_{\epsilon}(a\mid S_{t+1})Q(S_{t+1},a).
\\]

The canonical distribution assigns `epsilon / |A(s)|` to every action. It adds `1 - epsilon` to the first greedy action.

```haskell
updated <-
  updateExpectedSarsa
    rate
    discount
    model
    observedTransition
    (ExpectedSarsaContinuing epsilon)
    table
```

Use `ExpectedSarsaTerminal` for a terminal successor. The update uses an expectation and does not sample a next action for its target.

## Q-learning

Q-learning has a greedy off-policy continuing target:

\\[
y_t=r_{t+1}+\gamma\max_{a'}Q(S_{t+1},a').
\\]

```haskell
updated <- updateQ rate discount model observedTransition table
```

The maximum uses only actions exposed by the model. The episodic behavior can remain epsilon-greedy, but `updateQ` does not receive epsilon.

This separation keeps behavior and target policies distinct.

## Common terminal update

All four tabular algorithms use the same terminal target:

\\[
y_t=r_{t+1}+\gamma g(S_{t+1}).
\\]

A terminal update does not request a policy, epsilon value, next action, or terminal action support.

## Canonical epsilon-greedy behavior

Construct or sample the shared behavior distribution:

```haskell
distribution <-
  epsilonGreedyDistribution epsilon table state availableActions

(selected, nextGenerator) <-
  sampleEpsilonGreedy epsilon table state availableActions generator
```

Exact ties retain the first action in model availability order. Duplicate available actions are rejected.

## Bounded episodic runners

Each algorithm has an empty-start runner and a resumable `...From` runner:

| Algorithm | Empty start | Resume |
| --- | --- | --- |
| Q-learning | `learnEpisodes` | `learnEpisodesFrom` |
| TD(0) | `learnTD0Episodes` | `learnTD0EpisodesFrom` |
| SARSA | `learnSarsaEpisodes` | `learnSarsaEpisodesFrom` |
| Expected SARSA | `learnExpectedSarsaEpisodes` | `learnExpectedSarsaEpisodesFrom` |

A Q-learning configuration is explicit:

```haskell
config =
  qLearningConfig
    discount
    (ConstantLearningRate alpha)
    (ConstantExploration epsilon)
    episodeLimit
    episodeStepLimit

result <- learnEpisodes config model generator
```

A resumable call also receives the current table, episode index, update count, and generator:

```haskell
resumed <-
  learnEpisodesFrom
    config
    model
    previousTable
    nextEpisodeIndex
    previousUpdateCount
    previousGenerator
```

The other runners follow the same ownership rule. TD(0) also receives its supplied policy.

Every runner returns the updated table, bounded episode history, update count, and next generator. Splitting a run at an episode boundary preserves the seeded result when all resume fields are passed unchanged.

Zero episodes perform no work. A zero step limit can still collect an initial terminal payoff, because terminal inspection occurs first.

## Failure and atomicity boundary

The pure functions validate the supplied observation, source action availability, successor status and action support, and table arithmetic. They do not evaluate the transition kernel. Sampled execution validates model distributions when it calls the shared sampled-step operation. Both layers reject nonfinite values at their arithmetic boundaries.

A failed pure update returns no modified table. A runner returns `Left` rather than a partial result when a later operation fails.

## Claim boundary

These modules provide deterministic algebraic and seeded execution references for small finite problems. They do not claim convergence, sample efficiency, scalability, or production training behavior.

Tests compare exact seeded actions, transitions, targets, tables, and generator states. They do not use frequency or convergence thresholds.

## Further reading

- [Sutton: temporal-difference learning](references.md#sutton-temporal-difference-learning)
- [Sutton and Barto: reinforcement learning](references.md#sutton-and-barto-reinforcement-learning)
- [Watkins and Dayan: Q-learning](references.md#watkins-and-dayan-q-learning)
- [Rummery and Niranjan: SARSA](references.md#rummery-and-niranjan-sarsa)
- [van Seijen and colleagues: Expected SARSA](references.md#van-seijen-and-colleagues-expected-sarsa)
