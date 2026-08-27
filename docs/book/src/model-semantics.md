# Rewards, terminals, policies, and objectives

## Transition rewards

An MDP transition produces a joint outcome:

```haskell
exactTransitionOutcome reward successor
```

This type preserves the relationship between a branch reward and its successor.

For example, a successful repair can produce high reward and a healthy state. A failed repair can produce a cost and a damaged state.

Do not store these as independent distributions. That model would permit impossible reward and successor combinations.

## Terminal payoffs

A state has one explicit status:

```haskell
ExactContinuing
ExactTerminal payoff
```

A terminal payoff is not a transition reward. The evaluator reads it when execution reaches the terminal state.

The evaluator clamps terminal values during dynamic programming and Bellman iteration. It never asks a terminal state for available actions.

## Stable action IDs

An `ActionId action` names an action. The ID selects a transition distribution.

The action ID is not a sampled outcome. The transition outcome contains only the reward and successor state.

This separation prevents a branch weight from being confused with an action probability.

## Policies

A policy maps a state to a distribution over action IDs:

```haskell
exactPolicy :: ExactKernel state (ActionId action) -> ExactPolicy state action
```

Before evaluation, the framework checks the policy support against the actions available in that state.

`closeExactPolicy` integrates action selection into the MDP. The result preserves the joint reward and successor distribution.

## Objectives

An objective contains a horizon and discount:

```haskell
exactFiniteObjective horizon discount
```

The horizon counts remaining transitions. The discount applies once after each transition.

The finite discount domain permits values from zero through one. The Bellman contraction domain requires a value strictly less than one.

## Stop reasons

A sampled or enumerated trace records one stop reason:

```haskell
TerminalStop payoff
HorizonStop
```

A horizon stop in a continuing state does not invent a terminal payoff. A terminal stop records the payoff used in the return.

## Practical review checklist

Before you accept a model, answer these questions:

1. Does each reward belong to a transition or a terminal state?
2. Does each stochastic branch contain both reward and successor?
3. Does each continuing state have the intended action support?
4. Does each policy select only available actions?
5. Does the horizon count transitions rather than visited states?
6. Does the observation model describe the successor state?
