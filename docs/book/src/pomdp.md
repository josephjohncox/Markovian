# POMDP filtering and planning

A POMDP combines a latent exact MDP, an initial belief, and an observation kernel.

## Observation timing

Markovian fixes observations after the transition:

```text
current latent state
        |
        | action and transition
        v
successor latent state
        |
        | observation kernel
        v
observation
```

The observation distribution can depend on the selected action and successor state.

```haskell
pomdp = exactPOMDP model initialBelief observe
```

`exactObservationTiming` returns `ObserveAfterTransition`.

## Prediction

Prediction integrates the transition over the current belief:

```haskell
predicted <- predictExactBelief pomdp action prior
```

\\[
\hat b(s')=
\sum_s b(s)P(s'\mid s,a).
\\]

## Conditioning

Conditioning weights each predicted state by the observation likelihood:

```haskell
posterior <-
  conditionExactBelief pomdp action observation predicted
```

\\[
b'(s')=
\frac{O(o\mid a,s')\hat b(s')}
     {\sum_t O(o\mid a,t)\hat b(t)}.
\\]

The operation returns `ImpossibleExactObservation` when the denominator is zero. It does not select an arbitrary posterior.

Use `filterExactBelief` to perform prediction and conditioning in one call:

```haskell
posterior <- filterExactBelief pomdp action observation prior
```

## Worked observation update

Assume a robot predicts these floor states after `Move`:

\\[
P(\mathit{Wet})=\frac{3}{10},
\qquad
P(\mathit{Dry})=\frac{7}{10}.
\\]

Assume these sensor likelihoods:

\\[
P(\mathit{Slip}\mid\mathit{Wet})=\frac45,
\qquad
P(\mathit{Slip}\mid\mathit{Dry})=\frac1{20}.
\\]

After `Slip`, the wet-state posterior is:

\\[
\frac{(3/10)(4/5)}{(3/10)(4/5)+(7/10)(1/20)}=\frac{48}{55}.
\\]

## Belief-policy planning

A belief policy maps the current belief to an action distribution:

```haskell
policy = exactBeliefPolicy chooseAction
value  <- expectedExactBeliefReturn objective pomdp policy
```

The planner enumerates observations for a bounded horizon. It rejects states whose support mixes terminal and continuing behavior when that mixture makes one belief action contract invalid.

Use this planner for small exact references. Large POMDPs need approximate belief representations outside the semantic core.
