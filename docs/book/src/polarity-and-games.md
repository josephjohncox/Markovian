# Polarity, push-pull duality, and games

This chapter explains one pattern that appears in logic, probability, differentiation, and learning: information can travel in two typed directions.

The pattern is useful, but the instances are not one universal operation. A probability state moves forward through a channel. A payoff moves backward by conditional expectation. A tangent moves forward through a derivative. A cotangent pulls backward through the dual of a derivative. A game move belongs to one of two roles. Logical polarity controls which side chooses the next proof step.

The common lesson is about interfaces. A type must record what flows, which way it flows, and which laws support the reversal.

The state-payoff and tangent-cotangent sections form the core Markovian route. The logic, optics, and game sections are comparative extensions. A reader can skip those extensions on a first pass.

## One arrow, two induced transformations

Let `K : X -> Y` be a finite stochastic channel with entries `K(y | x)`.

A state on `X` is a probability distribution `p`. The channel pushes it forward to a state on `Y`:

\\[
(K_{\ast}p)(y)=\sum_x p(x)K(y\mid x).
\\]

A real-valued payoff on `Y` is a function `u : Y -> R`. A probabilistic predicate is the bounded special case `u : Y -> [0,1]`. The channel pulls either function backward to `X` by conditional expectation:

\\[
(K^{\ast}u)(x)=\sum_y K(y\mid x)u(y).
\\]

The two operations satisfy a pairing law:

\\[
\langle K_{\ast}p,u\rangle_Y=\langle p,K^{\ast}u\rangle_X.
\\]

For finite carriers, this equation expands to the same double sum on both sides:

\\[
\sum_y\left(\sum_xp(x)K(y\mid x)\right)u(y)=\sum_xp(x)\left(\sum_yK(y\mid x)u(y)\right).
\\]

This is the basic state-predicate duality. States move with the arrow. Payoffs move against it.

The notation `K_*` and `K^*` is conventional, but overloaded. It does not mean that every channel has an inverse or dagger.

## Pushforward is prediction

A `Prior x` is an exact finite state. A `StochasticMatrix NonNegativeRational x y` is a channel. The function `pushforward` composes them.

This operation models:

- one transition of a Markov chain;
- prediction through an observation channel;
- propagation of a belief before conditioning;
- marginalization after a joint-state construction.

Pushforward preserves normalization because the channel rows sum to one.

For a deterministic function `f : X -> Y`, pushforward collects all source mass with the same image:

\\[
(f_{\ast}p)(y)=\sum_{x:f(x)=y}p(x).
\\]

The operation can lose information. Many source states can have the same pushforward.

## Pullback is evaluation, not inversion

The backward operation answers a different question. Given a future payoff `u`, what expected payoff does each current input induce?

For a deterministic function, pullback is precomposition:

\\[
(f^{\ast}u)(x)=u(f(x)).
\\]

For a stochastic channel, it is conditional expectation:

\\[
(K^{\ast}u)(x)=\mathbb{E}[u(Y)\mid X=x].
\\]

No posterior appears. The result is a function on `X`, not a probability distribution on `X`.

This distinction prevents a common error. Pulling a payoff backward through a channel is not Bayesian inversion. Bayesian inversion requires a prior:

\\[
p(x)K(y\mid x)=q(y)K^{\sharp}_p(x\mid y).
\\]

The pointwise ratio determines an inverse only where `q(y)` is positive. At zero-evidence outputs, different versions can satisfy the balance equation. Markovian removes that ambiguity by restricting the inverse to positive support.

Markovian implements **payoff pullback** as `pullbackPayoff` in `Markovian.Category.Payoff.Exact`. It separately implements prior-indexed Bayesian inversion as `bayesianInverse`. An `ExactPayoff y` is a total signed `Rational`-valued function on an explicit finite set. Its checked table constructor rejects duplicate, outside, and missing labels. Pullback checks target-object alignment and returns an `ExactPayoff x`.

The three operations have different types:

| Operation | Input | Output | Extra data |
| --- | --- | --- | --- |
| State pushforward | State on `X` | State on `Y` | Channel `X -> Y` |
| Payoff pullback | Payoff on `Y` | Payoff on `X` | Channel `X -> Y` |
| Bayesian inversion | Observation on supported `Y` | Posterior channel to supported `X` | Prior and support |

## Bellman backup is a pullback with reward

Dynamic programming propagates value information backward.

For a policy-induced transition channel `P : S -> S` and value function `V`, the continuation term is

\\[
(P^{\ast}V)(s)=\sum_{s'}P(s'\mid s)V(s').
\\]

A Bellman operator adds reward and discount:

\\[
(TV)(s)=r(s)+\gamma(P^{\ast}V)(s).
\\]

Markovian stores reward and successor together. Its actual backup therefore uses outcomes rather than an independently averaged reward:

\\[
(TV)(s)=
\sum_{(r,s')}P(r,s'\mid s)
\left(r+\gamma V(s')\right).
\\]

This is pullback-like expectation plus explicit reward timing. It is not a categorical trace. The pullback law alone supplies neither fixed-point existence nor convergence. Infinite-horizon evaluation needs additional hypotheses, such as discount contraction or suitable properness or transience conditions. Finite-horizon evaluation uses bounded backward recursion instead.

The current APIs keep the split explicit:

```haskell
pushforward ::
    Eq y =>
    Prior x ->
    StochasticMatrix NonNegativeRational x y ->
    Either BayesianError (Prior y)

pullbackPayoff ::
    Eq y =>
    StochasticMatrix NonNegativeRational x y ->
    ExactPayoff y ->
    Either PayoffPullbackError (ExactPayoff x)

bayesianInverse ::
    (Eq x, Eq y) =>
    Prior x ->
    StochasticMatrix NonNegativeRational x y ->
    Either BayesianError (BayesianInverse x y)
```

`pairStatePayoff` accepts a normalized state matrix `1 -> X` and an `ExactPayoff X`. The exact fixtures check

\\[
\operatorname{pair}(K_{\ast}p,u)=\operatorname{pair}(p,K^{\ast}u).
\\]

Bellman interpreters still implement reward and discount inside their validated model contracts. The payoff API does not erase that additional structure.

## Tangent push and cotangent pull

Differentiation has a related typed duality.

For a smooth map `f : X -> Y`, the derivative at `x` pushes a tangent vector forward:

\\[
Df(x):T_xX\longrightarrow T_{f(x)}Y.
\\]

Dualizing the derivative gives the canonical cotangent pullback:

\\[
(Df_x)^{\ast}:T^{\ast}_{f(x)}Y
\longrightarrow T^{\ast}_xX.
\\]

Its defining pairing law is

\\[
\left((Df_x)^{\ast}\bar y\right)(v)=\bar y(Df_xv).
\\]

In chosen coordinates, the dual map is represented by the transpose Jacobian. An inner product can also identify tangents with cotangents, which gives the corresponding adjoint representation.

Forward-mode automatic differentiation composes tangent pushes. Reverse mode composes cotangent pulls in the opposite order.

This is why Markovian's neural reference package exposes VJPs:

```haskell
inputCotangent <- denseInputVJP network input outputCotangent
parameterCotangent <- denseParameterVJP network input outputCotangent
```

A cotangent is not a probability state. Its entries can be negative, and it need not normalize. The similarity to state-predicate duality is structural, not an identification of values.

## Diagonals reverse into accumulation

Suppose one parameter `p` is copied into two forward branches:

\\[
p\xmapsto{\Delta}(p,p).
\\]

The reverse map adds the two branch cotangents:

\\[
R[\Delta](p,(\bar p_1,\bar p_2))=\bar p_1+\bar p_2.
\\]

Forward sharing and backward accumulation are dual under the derivative pairing.

The same diagram does not justify copying arbitrary stochastic processes. A deterministic value can be shared. Two independent random executions require tensoring the process with itself. Markovian's purity-indexed circuits preserve this distinction.

## Logical polarity

Polarity in proof theory classifies connectives by the shape of their introduction rules and by who controls proof search.

In focused linear logic, a proof alternates between two phases:

- an asynchronous phase applies invertible rules without losing completeness;
- a synchronous phase focuses on one formula and applies non-invertible rules.

Positive and negative connectives determine these phases. Polarity reduces irrelevant interleavings in proof search. It does not declare one class good and the other bad.

The exact assignment depends on the logical system. In a common polarized classical linear-logic presentation, multiplicative tensor `⊗` and additive disjunction `⊕` are positive. Multiplicative par `⅋` and additive conjunction `&` are negative. Linear implication is derived from negation and par. Shift operators move a formula between polarities.

Do not transfer this classification mechanically to Haskell data types. Haskell's products, sums, functions, laziness, and effects live in a different operational setting. The useful design lesson is narrower:

> Make the producer-controlled and consumer-controlled parts of an interface explicit.

Call-by-push-value applies a related discipline. It separates value types from computation types. Values are inert data. Computations must execute and can perform effects or diverge. A computation of return type `F A` can produce a value of type `A`. This separation clarifies evaluation order and adjunctions between values and computations.

Markovian already follows part of this discipline operationally. Exact semantic values remain pure. Sampling, optimization, replay, and hardware execution belong to separate interpreters or packages.

## Variance and typed direction

A channel `K : X -> Y` induces two maps:

\\[
K_{\ast}:\mathrm{State}(X)\longrightarrow\mathrm{State}(Y),
\\]

\\[
K^{\ast}:\mathrm{Payoff}(Y)\longrightarrow\mathrm{Payoff}(X).
\\]

The state construction is covariant in the channel direction. The payoff construction is contravariant.

This variance is visible in Haskell types. A forward interpreter consumes an `X`-indexed value and produces a `Y`-indexed value. A backward interpreter accepts a `Y`-indexed request and produces an `X`-indexed request.

A future API could record direction at the type level:

```haskell
data Flow = Push | Pull

data Signal (flow :: Flow) boundary
```

Such an index cannot prove the mathematical laws by itself. It can prevent accidental composition of two interfaces with incompatible directions.

## Lenses and optics

A lens packages a forward observation with a backward update or request that uses retained context. In categorical learning, a parameterized lens or learner adds parameter state. Its backward part can return both input requests and parameter updates.

An optic generalizes this pattern through a residual object. Composition hides the intermediate residual while preserving the external interface.

This explains why lenses appear in categorical learning and compositional games. Both settings need a forward observation and a backward response. The interpretation of the backward value differs:

- a gradient learner sends a cotangent or update request;
- a bidirectional program can send a requested view update;
- an open game sends utility or continuation information;
- a probabilistic predicate transformer sends an expected payoff.

A shared optic shape does not make these payloads equal.

## Game semantics

Game semantics interprets a type as an arena of possible interaction.

Moves commonly carry two independent labels:

- **role:** Opponent or Player;
- **kind:** question or answer.

A legal play follows arena rules such as alternation, justification, and bracketing. A strategy selects Player behavior in response to legal histories.

The terminology is conventional. “Player” is the denoted program or proof. “Opponent” is its environment or context.

Composition connects two strategies along a shared arena. They interact internally, and the composite hides the internal moves. This is an operational account of typed composition, not ordinary function substitution.

In common linear game models, duality swaps Player and Opponent roles. A function-space arena is built from a dualized input and an output. Schematically:

\\[
A\multimap B\cong A^{\perp}\otimes B.
\\]

This equation is model-dependent. It must not be read as a claim about every Haskell function or every stochastic kernel.

## What game models can enforce

Different strategy conditions model different computational disciplines.

- **Determinism** limits Player to one response for a given history.
- **Receptivity** requires the strategy to accept legal Opponent moves.
- **Innocence** limits dependence to an appropriate visible view.
- **Well-bracketing** constrains answers to pending questions.
- **Totality** rules out some forms of premature deadlock.

These conditions are semantic assumptions, not automatic consequences of the arena.

They offer a useful analogy for Markovian protocols. An observation arrives after a transition. An action belongs to the policy. A stochastic outcome belongs to the environment model. Terminal states reject further steps. These ownership and timing rules resemble a typed interaction protocol.

The current library does not encode them as game-semantic arenas or strategies.

## MDPs are not automatically games

An MDP contains actions and stochastic outcomes, but it is not a two-player game merely because two kinds of choice occur.

A standard MDP has one decision-making policy and one fixed environment kernel. The kernel does not optimize against the policy.

A stochastic game needs more structure:

- explicit ownership of each decision;
- strategy spaces for multiple agents;
- an information pattern;
- payoffs for each agent;
- an equilibrium or adversarial solution concept.

Sampling an outcome is not an Opponent strategy. Calling it one would confuse chance with agency.

## Compositional open games

An open game of type `(X,S) -> (Y,R)` has a strategy set `Σ`, a play map `Σ × X -> Y`, and a coplay map `Σ × X × R -> S`. Its best-response relation depends on a context containing an input `x : X` and a continuation `k : Y -> R`. Open games compose sequentially and in parallel.

This architecture is relevant to Markovian because it separates:

- forward state and observation flow;
- backward objective information;
- local decision rules;
- open boundaries.

However, Markovian's structured cospans are topological objects, and its stochastic circuits denote channels. They do not carry best-response relations or equilibria. A future game layer would need new semantic data and new laws.

The practical opportunity is a typed protocol layer above the existing kernels. Such a layer could reuse finite carriers and exact outcomes while adding player ownership and solution concepts explicitly.

## A compact duality map

| Setting | Forward or positive flow | Backward or negative flow | Required structure |
| --- | --- | --- | --- |
| Probability | State pushforward | Payoff pullback | Normalized channel and expectations |
| Bayesian inference | Evidence generation | Prior-indexed inverse | Prior, positive support, disintegration |
| Differentiation | Tangent push | Cotangent pull | Derivative and dual pairing |
| Learning | Prediction | Request or gradient | Loss, reverse rule, optimizer |
| Focused logic | Synchronous phase | Asynchronous phase | Polarized proof system |
| Game semantics | Player/Opponent interaction | Role-swapped dual arena | Arena and strategy laws |
| Open games | State or play | Utility or coutility | Best-response semantics |

The table records analogies of shape. It does not assert one shared category or one universal reversal.

## Concrete consequences for Markovian

The push-pull view suggests changes that can be tested.

### Extend state and payoff transformers

The checked exact finite payoff pullback and representative pairing fixtures are implemented. Future state-like APIs can reuse the pairing contract when they expose a normalized singleton-source matrix. Reward-bearing Bellman refactoring requires separate timing and discount evidence.

### Factor Bellman code through payoff pullback

A common expectation operator could serve finite-horizon evaluation, policy evaluation, POMDP planning, and information objectives. Reward-successor correlation must remain explicit.

### Index protocol ownership

No interaction syntax is implemented. An ownership index alone would be decorative. A first finite protocol must also define legal and terminal histories, strategy behavior on every owned move, composition through a shared boundary, and observational equality. Until one such protocol is selected, this work remains blocked and cannot support claims about game semantics, open games, equilibria, or multi-agent stochastic games.

### Compile paired interpreters

A circuit compiler could produce both a forward state interpreter and a backward payoff interpreter. Differential circuits could produce the analogous primal and VJP pair.

### Use dual laws as differential tests

For exact finite values, test

\\[
\langle K_{\ast}p,u\rangle=\langle p,K^{\ast}u\rangle.
\\]

For approximate derivatives, test

\\[
\langle Df(x)v,\bar y\rangle
\approx
\langle v,Df(x)^{\mathsf T}\bar y\rangle.
\\]

These tests are cheap and expose orientation, layout, and transpose errors.

## Boundaries

This chapter does not claim:

- that state pushforward and payoff pullback form a Bayesian inverse;
- that a cotangent is a probability predicate;
- that logical polarity has one fixed meaning across all calculi;
- that Haskell's ordinary function arrow is a linear implication;
- that Markovian currently implements game semantics or open games;
- that an MDP is a two-player stochastic game;
- that open-system boundary reversal swaps strategic roles;
- that a shared diagram removes the need for domain-specific laws.

The grounded claim is smaller. Typed direction exposes who supplies information, where it travels, and which pairing law an implementation must satisfy.

## Reading route

1. Read [Girard](references.md#girard-linear-logic) for linear negation and resource-sensitive connectives.
2. Read [Andreoli](references.md#andreoli-focusing) for focusing and proof-theoretic polarity.
3. Read [Levy](references.md#levy-call-by-push-value) for the value-computation split.
4. Read [Kozen](references.md#kozen-probabilistic-programs) and [Jacobs and Zanasi](references.md#jacobs-and-zanasi-predicate-state-transformers) for state and predicate transformers.
5. Read [Hyland and Ong](references.md#hyland-and-ong-game-semantics) or [Abramsky, Jagadeesan, and Malacaria](references.md#abramsky-jagadeesan-and-malacaria-game-semantics) for PCF game models.
6. Read [Laurent](references.md#laurent-polarized-games) for polarized games.
7. Read [Riley](references.md#riley-categories-of-optics) and [Ghani and colleagues](references.md#ghani-and-colleagues-compositional-games) for optics and open games.
8. Return to [categorical learning](categorical-learning.md) for VJPs, diagonals, and optimization.
