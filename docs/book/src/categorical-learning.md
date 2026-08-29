# Categorical structure of learning and neural networks

A neural network is not made categorical by drawing boxes around its layers. The categorical content appears when the types of composition, parameter sharing, differentiation, and updates are stated precisely.

This chapter separates four structures that are often conflated:

1. composition of forward computations;
2. reverse propagation of sensitivities;
3. parameter updates chosen by an optimizer;
4. stochastic or Bayesian inference.

Markovian implements small finite reference instances of all four except a general automatic-differentiation language. The purpose of the categorical view is to identify laws and compiler opportunities, not to rename ordinary array programming.

See [Fong, Spivak, and Tuyéras](references.md#fong-spivak-and-tuyeras-backprop), [Cockett and colleagues](references.md#cockett-and-colleagues-reverse-derivatives), and [Cruttwell and colleagues](references.md#cruttwell-and-colleagues-gradient-learning). Read [Polarity, push-pull duality, and games](polarity-and-games.md) next for tangent-cotangent variance, logical polarity, and interaction protocols.

## Parameterized maps

An ordinary deterministic layer is a map

\\[
f:X\longrightarrow Y.
\\]

A trainable layer has a parameter object `P`:

\\[
f:P\otimes X\longrightarrow Y.
\\]

In Haskell, the simplest representation is morally

```haskell
newtype Parametric p x y = Parametric
  { runParametric :: p -> x -> y
  }
```

This type alone gives no differentiation, optimization, or probability. It only says that parameters and inputs are distinct arguments.

If

\\[
f:P\otimes X\to Y
\\]

and

\\[
g:Q\otimes Y\to Z,
\\]

then their parameterized composite has parameter object `P tensor Q`:

\\[
g\diamond f:
(P\otimes Q)\otimes X\longrightarrow Z.
\\]

Associators and symmetries account for the necessary rearrangement of wires. This is why a list of untyped parameter arrays is weaker than a typed parameterized circuit: the latter records which parameters belong to which component and how composition combines them.

## Losses are scalar-valued objectives, not reverse maps

For a prediction `yHat` and target `y`, a scalar loss is a scalar-valued map

\\[
\ell:Y\otimes Y\longrightarrow \mathbb{R}.
\\]

Examples include squared error and categorical cross entropy. Choosing a loss changes the learning problem. It is not determined by the forward network.

A complete supervised objective combines data, a parameterized predictor, and a loss:

\\[
P\otimes X\otimes Y
\xrightarrow{f\otimes\mathrm{id}}
Y\otimes Y
\xrightarrow{\ell}
\mathbb{R}.
\\]

The data distribution then turns this pointwise loss into an expectation.

## Forward derivatives and reverse derivatives

For a smooth map between finite-dimensional vector spaces,

\\[
f:X\to Y,
\\]

the derivative at `x` is a linear map

\\[
D f(x):X\to Y.
\\]

Forward mode applies this map to an input tangent `dx`:

\\[
(x,dx)\longmapsto
\bigl(f(x),D f(x)\\,dx\bigr).
\\]

Reverse mode applies the transpose or adjoint derivative to an output cotangent `dyBar`:

\\[
(x,\bar y)\longmapsto
D f(x)^{\mathsf T}\bar y.
\\]

The computational object used by Markovian is a vector--Jacobian product:

\\[
\operatorname{VJP}_f(x,\bar y)=
D f(x)^{\mathsf T}\bar y.
\\]

`denseParameterVJP` sends an output cotangent to parameter cotangents. `denseInputVJP` sends it to input cotangents. Neither function constructs the complete Jacobian.

## The reverse chain rule

For composable maps `f : X -> Y` and `g : Y -> Z`,

\\[
D(g\circ f)(x)=Dg(f(x))\\,Df(x).
\\]

Taking transposes reverses the order:

\\[
D(g\circ f)(x)^{\mathsf T}=
Df(x)^{\mathsf T}
Dg(f(x))^{\mathsf T}.
\\]

This is the mathematical reason backpropagation traverses layers backward. Starting with output cotangent `zBar`, compute

\\[
\bar y=Dg(f(x))^{\mathsf T}\bar z,
\\]

then

\\[
\bar x=Df(x)^{\mathsf T}\bar y.
\\]

The reverse pass is compositional because the chain rule is compositional. A category with a reverse-derivative operation axiomatizes this behavior without committing to one tensor library or tape representation.

## Four meanings of “adjoint” or “reverse”

The following operations are related by notation but are not interchangeable.

### Linear adjoint

Given inner products, a linear map `A : X -> Y` has an adjoint `A dagger : Y -> X` satisfying

\\[
\langle Ax,y\rangle_Y=
\langle x,A^{\dagger}y\rangle_X.
\\]

In Euclidean coordinates this is the transpose. Reverse-mode differentiation uses this operation on the derivative at a point.

### Reverse derivative

A reverse derivative depends on the primal point because a nonlinear derivative does:

\\[
R[f](x,\bar y)=Df(x)^{\mathsf T}\bar y.
\\]

It is not a global inverse and need not reconstruct `x` from `f(x)`.

### Adjoint functors

A categorical adjunction `F leftAdjoint G` is a natural correspondence of hom-sets:

\\[
\mathcal{D}(F X,Y)
\cong
\mathcal{C}(X,G Y).
\\]

This is a different level of structure from a matrix transpose. An autodiff implementation does not become an adjoint functor merely because it runs backward.

### Bayesian inversion

A Bayesian inverse reverses a stochastic channel relative to a prior and only on positive evidence:

\\[
p(x)K(y\mid x)=q(y)K^{\sharp}_p(x\mid y).
\\]

It is neither a derivative transpose nor an adjoint functor. Markovian keeps all four notions separate in its APIs.

## Diagonals, parameter sharing, and gradient accumulation

The diagonal duplicates a value:

\\[
\Delta_X:X\longrightarrow X\otimes X,
\qquad
x\longmapsto(x,x).
\\]

A shared parameter used by two branches is represented by a diagonal before the branches. If the total loss receives cotangent contributions `pBar1` and `pBar2`, the reverse derivative of the diagonal adds them:

\\[
R[\Delta_X](p,\bar p_1,\bar p_2)=
\bar p_1+\bar p_2.
\\]

This is not an implementation accident. Forward copying and reverse accumulation are dual parts of the derivative rule. It explains why tied neural-network weights receive the sum of gradients from every use.

Discard has the complementary rule. A forward value that does not affect the result receives zero cotangent in reverse:

\\[
R\\!\left[!_X\right]\\,(x,())=0.
\\]

These rules provide immediate compiler checks:

- every forward use of a shared parameter must contribute exactly once in reverse;
- dead forward values must not manufacture a gradient;
- changing sharing into independent parameter copies changes the model;
- summing branch gradients is required before one atomic parameter update.

## Determinism controls safe duplication

In a cartesian deterministic fragment,

\\[
\Delta_Y\circ f=
(f\otimes f)\circ\Delta_X.
\\]

This equation permits two execution plans:

1. compute `f` once and share the result;
2. copy the input and compute `f` twice.

The first plan can avoid duplicate deterministic work. Markovian's `ShareCircuit` retains the one-computation syntax, while `copyNaturalDeterministic` constructs the law-equivalent fanout form only for proof-carrying deterministic circuits.

For a stochastic `f`, the rewrite is unsound. Computing once and sharing preserves one random draw; computing twice creates two draws. The purity index therefore acts as an optimization capability, not decorative metadata.

## Neural layers as composable maps

An affine layer has

\\[
y=W x+b.
\\]

Given output cotangent `yBar`, reverse propagation yields

\\[
\bar x=W^{\mathsf T}\bar y,
\\]

\\[
\bar W=\bar y\\,x^{\mathsf T},
\qquad
\bar b=\bar y.
\\]

For the elementwise hyperbolic tangent,

\\[
y_i=\tanh x_i,
\\]

\\[
\bar x_i=(1-y_i^2)\bar y_i.
\\]

Composing these rules gives the VJP for a dense network. Markovian's neural package implements the composite manually and checks every represented parameter derivative with central finite differences.

## Softmax, information, and gradients

For logits `z`, softmax is

\\[
q_i=\frac{e^{z_i}}{\sum_j e^{z_j}}.
\\]

Its Jacobian is

\\[
\frac{\partial q_i}{\partial z_j}
=q_i(\mathbf{1}_{i=j}-q_j).
\\]

For target distribution `p` and cross entropy

\\[
\mathcal{L}(z)=-\sum_i p_i\log q_i,
\\]

the composite reverse derivative simplifies to

\\[
\frac{\partial\mathcal{L}}{\partial z_j}=q_j-p_j.
\\]

This fusion is both a proof and an optimization. A backend need not materialize the softmax Jacobian and then multiply it by a loss gradient. It can compute `q - p` directly, with improved cost and usually improved numerical behavior.

`crossEntropyPredictionGradient` implements this fused rule. A finite-difference fixture checks it against the unfused scalar objective.

## Score-function gradients for stochastic nodes

A sampled categorical action is not differentiated by pretending the discrete sample is a smooth map. For a parameterized policy `piTheta`, the score-function identity is

\\[
\nabla_\theta
\mathbb{E}_{A\sim\pi_\theta}[G(A)]=
\mathbb{E}
\left[
G(A)\nabla_\theta\log\pi_\theta(A)
\right]
\\]

when the usual interchange and support assumptions hold.

For a softmax policy, the selected-action logit score is

\\[
\frac{\partial\log\pi(a)}{\partial z_j}=
\mathbf{1}_{a=j}-\pi(j).
\\]

REINFORCE uses this estimator. Actor--critic substitutes a bootstrapped advantage estimate. The stochastic estimator belongs at the stochastic node; ordinary reverse derivatives still apply inside the deterministic network that computes the logits.

This gives a clean hybrid diagram:

1. deterministic network: reverse-mode VJP;
2. categorical sampling: score-function estimator;
3. environment kernel: sampled transition, not differentiated;
4. scalar return or advantage: cotangent-like learning signal;
5. optimizer: explicit parameter update.

## Optimizers are additional dynamics

A gradient is not an update. Stochastic gradient descent chooses

\\[
\theta_{t+1}=\theta_t-\eta_t g_t.
\\]

This requires a learning rate, a sign convention, and an identification of cotangents with parameter displacements. Momentum, Adam, natural gradient, and mirror descent add different state or geometry.

Consequently, optimization does not follow “for free” from category composition. What does come close to free is compositional gradient construction once each primitive supplies a valid reverse rule.

Markovian's `applySGD` is intentionally separate from `denseParameterVJP`. The first changes parameters; the second computes a sensitivity. Failed checked arithmetic prevents the update from becoming partially visible.

## Learning as a compositional bidirectional system

A learner or parametric lens packages a forward implementation with backward behavior. Schematically it contains:

- an implementation `P tensor X -> Y`;
- a parameter update map using a backward signal;
- a request map that propagates a signal toward earlier components.

Composition connects the request from a later learner to the earlier learner and combines parameter states. Parallel composition tensors both forward and backward behavior.

Fong, Spivak, and Tuyéras show that gradient descent and backpropagation can be organized functorially under explicit assumptions. Reverse-derivative categories and parametric lenses refine which reverse laws are required.

Markovian does not currently expose a universal `Learner` type. Its dense networks, policies, and update functions are executable finite witnesses of the same separation:

```haskell
prediction <- denseForward network features
parameterCotangent <- denseParameterVJP network features outputCotangent
inputCotangent <- denseInputVJP network features outputCotangent
updated <- applySGD optimizer parameterCotangent network
```

## Reasoning and efficiency available now

### 1. VJP composition avoids full Jacobians

If a network has many parameters and one scalar objective, reverse mode computes one cotangent propagation instead of one forward derivative per parameter. The categorical chain rule determines the order of composition.

### 2. Shared deterministic computations can be evaluated once

A `ShareCircuit` stores one result and copies its value. Copy naturality proves equivalence with duplicated deterministic execution, but the shared syntax can be cheaper. The same rewrite is forbidden for stochastic primitives, preserving correlation.

### 3. Shared parameters imply exact gradient accumulation structure

The reverse rule for the diagonal says where branch gradients must add. This can detect missing, duplicated, or prematurely applied parameter updates.

### 4. Softmax and cross entropy can be fused

The composite gradient `q - p` avoids constructing a dense Jacobian. The implementation and its finite-difference test turn a textbook identity into an executable optimization contract.

### 5. Exact denotations can test approximate learners

For a finite model, an exact kernel or Bellman solver supplies target probabilities and values. A neural approximator can be checked through a commuting-square fixture:

\\[
\begin{array}{ccc}
\text{exact input} & \xrightarrow{\text{exact semantics}} & \text{exact output} \\\\
\downarrow\text{lower} && \downarrow\text{observe} \\\\
\text{floating input} & \xrightarrow{\text{approximate backend}} & \text{floating output}.
\end{array}
\\]

The square commutes only up to the declared observation relation and tolerance. This catches timing, masking, support, and layout errors that a training curve can hide.

### 6. Information laws provide metamorphic tests

Without knowing one expected scalar in advance, tests can check:

- entropy invariance under a common logit shift;
- entropy additivity for independent product logits;
- zero self-KL within tolerance;
- data-processing inequalities on finite channels;
- zero-sum gradients along the softmax shift symmetry.

These are cheap law-derived tests, not statistical convergence claims.

## New deductions from combining the layers

The following deductions preview the consolidated catalogue in [Derived categorical and mathematical insights](categorical-insights.md#12-one-diagonal-governs-sharing-forward-and-accumulation-backward). They remain here to show how they arise from the learning construction.

### Sharing has both probabilistic and differential semantics

The same diagonal means “one value with two consumers” in the forward stochastic circuit and “sum both consumer sensitivities” in reverse differentiation. This unifies correlation accounting and gradient accounting. Replacing sharing with repetition changes both the joint distribution and the update.

### Purity evidence is an optimization certificate

A deterministic purity witness authorizes common-subexpression elimination across copy. A stochastic node lacks that certificate because re-execution changes independence. The type index therefore guards a real optimizer boundary.

### Bayesian and gradient reversals can be composed without being identified

A probabilistic layer can first infer a posterior by prior-indexed Bayesian inversion. A differentiable parameterization can then optimize a loss derived from that posterior. The two reverse passes require different evidence and equality notions. Keeping them separate allows a pipeline to use both safely.

### Frontier width and reverse storage expose related graph separators

Acyclic probabilistic elimination stores a live frontier of random variables. Reverse-mode differentiation stores or recomputes primal intermediates needed by the backward pass. Both costs depend on graph separation and schedule, although one performs sum--product elimination and the other applies chain-rule transposes. This suggests a shared graph-analysis layer, not a shared semantic operator.

### Exact support can constrain approximate optimization

An exact finite channel identifies impossible actions or observations before a floating learner runs. Masks can therefore be compiled from exact support, guaranteeing that softmax normalization and argmax operate only on semantically available actions. This removes one class of invalid exploration and target errors almost for free.

## Near-term Haskell extensions

The current types support several conservative additions.

1. **A parametric-circuit layer** with typed parameter products and explicit sharing.
2. **Primitive reverse rules** returning input and parameter VJPs, interpreted compositionally.
3. **A checked optimizer state** separate from the differentiated program.
4. **A graph-cost interpreter** over `CircuitAlgebra` that counts primitive work, copied values, and maximum live width before execution.
5. **A rewrite certificate** recording which deterministic copy, identity, associativity, or fusion law justified an optimization.
6. **Exact-support-generated masks** for neural policy and DQN examples.
7. **Commuting-square differential tests** for every approximate interpreter.

The first acceptable implementation should remain finite and framework-independent. A tensor framework or GPU autodiff backend can implement the same interface later; it should not define the semantics.

## Boundaries

This chapter does not claim:

- that every neural architecture forms a cartesian or compact category;
- that reverse differentiation is Bayesian inversion;
- that an adjoint functor is a matrix transpose;
- that gradient descent is coordinate-free without a chosen metric or update geometry;
- that categorical organization proves convergence or improves predictive accuracy;
- that floating reassociation preserves exact denotation;
- that stochastic sampling can be differentiated as an ordinary smooth map;
- that an optimizer rewrite is sound without purity, shape, and numerical evidence.

The categorical gain is compositional proof obligation and lawful reuse. Runtime gains occur only when those laws justify a concrete transformation such as VJP composition, deterministic sharing, fusion, elimination, or support masking.

The next chapter places this backward flow beside payoff transformers, logical polarity, and game semantics. It keeps their shared interface shape separate from their different payloads and laws.
