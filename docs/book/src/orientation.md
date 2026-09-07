# Orientation and notation

**Prerequisites:** finite probability and elementary linear algebra.
**Outcome:** translate between mathematical expressions, public API arguments,
and checked representation boundaries. **Next:** choose a [learning route](learning-routes.md)
or try the [law laboratory](law-laboratory.md).

## Kernels and matrices

A kernel `K : X -> Y` assigns a distribution on Y to each x. We write
`K(y | x)` for its mass at y. `Matrix scalar source target` stores **source rows,
target columns**, so its `(x,y)` entry is `K(y | x)`, not `K(x | y)`.
A prior is a row vector. Its pushforward is `p K`.

`composeMatrix k l` executes k first, then l. Its entry at `(x,z)` is the
sum over y of `k(x,y) * l(y,z)`. Conventional Kleisli notation is `K >=> L`.
Function composition `g . f` instead places the first operation on the right.
Never infer execution order merely from the word “composition”.

The root exact distributions deliberately have **no Monad instance**.
`bindExactFiniteDistChecked` requires explicit limits and can reject execution.
Algebraic denotation alone does not establish identical resource admission.
The later EL-06 experiment will address that boundary; it is not covered here.

## Time and joint outcomes

An MDP step first inspects status. A terminal state stops and contributes its
payoff even if the remaining horizon is zero. A continuing state with zero
remaining horizon stops without a terminal payoff. Otherwise the policy selects
an available action and the kernel draws **one joint reward/successor outcome**.
The immediate reward is at the current time; future return is discounted.

Thus one transition with reward r into terminal payoff t returns `r + gamma*t`.
An action selected, a transition realized, an observation received and a terminal
stop are different events. The [POMDP chapter](pomdp.md) specifies observation
conditioning after prediction; the [feedback chapter](feedback.md) explicitly
represents durations. Do not identify duration with a matrix traversal count.

## Four equality questions

| Question | Meaning |
| --- | --- |
| Exact denotation | Literal equality of Rational values, or value-indexed matrix entries after support alignment |
| Approximate numerical agreement | A stated absolute/relative tolerance and finite input domain; not exact associativity |
| Layout equality | The same canonical ordering as well as the same represented labels |
| Storage identity | The same underlying tensor storage; not necessarily the same logical coordinate order |

`sameFiniteSupport` ignores ordering; `sameFiniteLayout` does not. The analogous
matrix operations distinguish value-indexed equivalence and literal layout.
A transposed tensor can share storage with a differently ordered base, while a
materialized copy has equal coordinates but different storage. Do not use an
approximate comparison to discharge an exact or nominal-owner proof obligation.

## Three different reverse operations

1. **Matrix transpose / payoff pullback.** Transpose interchanges indices.
   Pulling a payoff g back along K computes the source function whose value is
   the expectation of g under K at that source. No prior or Bayesian division
   is involved. A transpose of a stochastic matrix need not be stochastic.
2. **Bayesian inverse.** Given prior p and positive evidence at y, its mass at x
   is `p(x)*K(y | x) / (p K)(y)`. The inverse depends on p and is represented on
   the supported source and observation objects. Zero evidence is a checked
   failure, not permission to invent a posterior.
3. **VJP (transposed Jacobian action).** For a differentiable deterministic
   function f at input x, a VJP sends output cotangent c to `Df(x)^T c` using
   the specified finite Euclidean pairing. It depends on the primal x and on
   which inputs are held fixed. It is neither stochastic reversal nor a
   posterior. A primitive supplied a view differentiates its logical inputs;
   pulling further back to a base requires the view map's own chain rule.

**Exercise:** which reverse operation needs a prior? **Hint:** ask which one
normalizes by observation evidence. **Checked answer:** Bayesian inversion;
[laboratory B](law-laboratory.md#b-transpose-is-not-bayesian-inversion) gives a
nonuniform-prior calculation and an explicit zero-evidence failure. Laboratory F
checks the distinct coordinate issue for a VJP.
