# Information theory for finite stochastic models

Information theory gives numerical answers to questions that categorical probability first expresses structurally:

- how uncertain is a state?
- how much does an observation reveal?
- how far is one predictive distribution from another?
- what information can a channel destroy?
- when does a learned representation preserve what a controller needs?

Markovian's exact core can represent all relevant finite distributions and channels. Most Shannon quantities contain logarithms and are generally irrational even when every probability is rational. Their numerical evaluation therefore belongs in an explicit approximate interpreter, not in the exact rational semantic core.

See [Shannon](references.md#shannon-information-theory), [Cover and Thomas](references.md#cover-and-thomas-information-theory), [Baez, Fritz, and Leinster](references.md#baez-fritz-and-leinster-entropy), and [Perrone](references.md#perrone-markov-categories-and-entropy).

## Self-information and entropy

For an event with positive probability `p(x)`, its self-information in nats is

\\[
I(x)=-\log p(x).
\\]

Using logarithm base two gives bits instead. Shannon entropy is expected self-information:

\\[
H(X)=-\sum_x p(x)\log p(x).
\\]

The convention `0 log 0 = 0` is justified by the limit from positive probabilities. Entropy is zero for a point mass and maximal at the uniform distribution on a fixed finite support.

Entropy is not uncertainty attached to a bare type. It depends on a state `p : I -> X`. The same finite carrier can support a deterministic prior, a uniform prior, or anything between them.

## Joint, marginal, and conditional entropy

For a joint state `p(x,y)`, the marginals are obtained by discarding one output:

\\[
p_X(x)=\sum_y p(x,y),
\qquad
p_Y(y)=\sum_x p(x,y).
\\]

The joint entropy and conditional entropy are

\\[
H(X,Y)=-\sum_{x,y}p(x,y)\log p(x,y),
\\]

\\[
H(Y\mid X)=
-\sum_{x,y}p(x,y)\log p(y\mid x).
\\]

On positive support they satisfy the chain rule

\\[
H(X,Y)=H(X)+H(Y\mid X).
\\]

In Markovian, a prior `p : I -> X` and a channel `K : X -> Y` create the joint

\\[
p(x,y)=p(x)K(y\mid x).
\\]

This is the finite categorical form of sampling `X` and then sampling `Y` conditionally on `X`.

## Mutual information

Mutual information measures the dependence in a joint state:

\\[
I(X;Y)=
\sum_{x,y}p(x,y)
\log\frac{p(x,y)}{p_X(x)p_Y(y)}.
\\]

Equivalent forms are

\\[
I(X;Y)=H(X)+H(Y)-H(X,Y)
\\]

and

\\[
I(X;Y)=H(Y)-H(Y\mid X).
\\]

These equations make the circuit distinction between sharing and repetition quantitative.

Let `X` be one fair bit.

- `shareCircuit coin` produces `(X,X)`. The pair has entropy `H(X,X)=H(X)` and mutual information `I(X;X)=H(X)`.
- `fanoutCircuit coin coin` produces independent `(X_1,X_2)`. Its entropy is `H(X_1,X_2)=2H(X)` and its mutual information is zero.

The wiring diagram determines whether information is shared. No post-hoc correlation flag is needed.

## Cross entropy and relative entropy

For target distribution `p` and predictive distribution `q`, cross entropy is

\\[
H(p,q)=-\sum_x p(x)\log q(x).
\\]

Kullback--Leibler divergence is

\\[
D_{\mathrm{KL}}(p\Vert q)=
\sum_x p(x)\log\frac{p(x)}{q(x)}.
\\]

When `q(x)>0` wherever `p(x)>0`,

\\[
H(p,q)=H(p)+D_{\mathrm{KL}}(p\Vert q).
\\]

Gibbs' inequality gives

\\[
D_{\mathrm{KL}}(p\Vert q)\ge 0,
\\]

with equality exactly when the finite distributions agree. If `q` assigns zero mass to a positive-mass target event, the divergence is infinite. A finite-logit softmax has strictly positive mathematical probabilities, although floating exponentiation can underflow and must still be checked.

KL divergence is not a metric: it is asymmetric and does not satisfy the triangle inequality.

## Channels and the data-processing inequality

Suppose `X -> Y -> Z` is a Markov chain, so `Z` is obtained by postprocessing `Y`. Information cannot increase merely by postprocessing:

\\[
I(X;Z)\le I(X;Y)
\\]

for a Markov chain `X -> Y -> Z`.

Equivalently, applying one channel `K` to both arguments contracts relative entropy:

\\[
D_{\mathrm{KL}}(pK\Vert qK)
\le
D_{\mathrm{KL}}(p\Vert q).
\\]

In finite probability this follows from the log-sum inequality. Categorically, the inequality says that divergence is monotone under channel composition. Equality requires additional sufficiency conditions; it is not automatic.

This gives a useful test obligation for representation learning. If an encoder discards distinctions, no decoder can recreate information that was absent from the encoded state. A successful task-specific representation need not preserve all information, but it must preserve information sufficient for the task.

## Sufficient statistics and Bayesian inversion

A deterministic statistic `T : X -> S` is sufficient for a parameter or latent variable when conditioning through `S` loses no relevant inferential information. In finite form, this becomes a factorization or conditional-independence statement.

Bayesian inversion answers a different question from entropy. Given prior `p` and channel `K`, it constructs a posterior channel on positive evidence:

\\[
p(x)K(y\mid x)=q(y)K^{\sharp}_p(x\mid y).
\\]

Entropy can then quantify expected posterior uncertainty:

\\[
H(X\mid Y)=
\sum_y q(y)H\bigl(K^{\sharp}_p(-\mid y)\bigr).
\\]

The expected information gain is

\\[
I(X;Y)=H(X)-H(X\mid Y).
\\]

Thus prior-indexed Bayesian inversion supplies the posterior family, while information theory assigns a scalar summary. Neither operation replaces the other.

## Convex mixtures and latent choices

Let a latent variable `Z` select component distribution `p_z` with weight `lambda_z`:

\\[
p(x)=\sum_z\lambda_z p_z(x).
\\]

The entropy of the visible mixture obeys

\\[
\sum_z\lambda_z H(p_z)
\le H(X)
\le H(Z)+\sum_z\lambda_z H(p_z).
\\]

The lower gap is the information that `X` carries about the hidden component. The upper bound states that hiding the component label cannot create more uncertainty than the label itself contributes.

This is the information-theoretic form of the book's distinction between one shared latent choice and independent choices in separate tensor factors.

## Information objectives in learning

### Maximum likelihood and cross entropy

For supervised categorical data with empirical target `p` and model `q_theta`, minimizing negative log likelihood is minimizing cross entropy:

\\[
\mathcal{L}(\theta)=
-\mathbb{E}_{x\sim p}\log q_\theta(x).
\\]

For softmax logits `z` and target probabilities `p`, the prediction-logit gradient is

\\[
\frac{\partial H(p,q)}{\partial z_j}=q_j-p_j.
\\]

This compact gradient is a consequence of composing log-softmax with cross entropy. It is not a general rule for arbitrary output maps and losses.

### Entropy regularization

A policy objective can reward both return and conditional action entropy:

\\[
J_{\mathrm{ent}}(\theta)=
\mathbb{E}_{\pi_\theta}[G]
+
\alpha\\,
\mathbb{E}_{S}[H(\pi_\theta(-\mid S))].
\\]

The coefficient `alpha` changes the optimization problem. Entropy regularization is not a semantics-preserving rewrite of the original MDP objective.

### KL constraints

Trust-region and proximal methods restrict how far a new policy moves from a reference policy, often through an expected KL term:

\\[
\mathbb{E}_{S}
\left[
D_{\mathrm{KL}}
\bigl(\pi_{\mathrm{old}}(-\mid S)
\Vert
\pi_\theta(-\mid S)
\bigr)
\right].
\\]

The state distribution, KL direction, support policy, and approximation tolerance are part of the contract. Reversing the KL arguments changes the objective.

## Executable Haskell boundary

The optional neural package evaluates information quantities from stable logits:

```haskell
entropy <- entropyFromLogits logits
crossEntropy <- crossEntropyFromLogits targetLogits predictionLogits
divergence <- klDivergenceFromLogits sourceLogits targetLogits
mutualInformation <-
  mutualInformationFromJointLogits leftSize rightSize jointLogits

dEntropy <- entropyLogitGradient logits
dCrossEntropy <-
  crossEntropyPredictionGradient targetLogits predictionLogits
```

`Markovian.Backend.Neural.Information` uses checked `Double` arithmetic. Its fixtures verify:

- uniform entropy equals `log 2` for two equal logits;
- entropy is invariant under a common logit shift;
- entropy is additive for independent product logits;
- cross entropy decomposes as entropy plus KL on finite fixtures;
- self-KL is numerically zero within an explicit tolerance;
- independent product logits have zero mutual information;
- one shared fair bit has mutual information `log 2`;
- analytic entropy and cross-entropy gradients match central finite differences;
- both gradients are orthogonal to the all-ones logit-shift direction.

The exact layer remains the oracle for probabilities, support, tensor, sharing, and channel composition. The information layer approximates logarithmic observables of that exact structure.

## What is exact and what is approximate?

The following statements are structural or exact for validated finite distributions:

- construction of joints and marginals;
- independence as tensor factorization;
- equality and almost-sure equality on named support;
- whether a deterministic statistic factors a channel;
- whether shared and independent circuits denote the same distribution.

The following generally require approximation:

- Shannon entropy in nats or bits;
- KL divergence;
- cross entropy;
- mutual information expressed through logarithms;
- gradients of these quantities.

A rational input does not make its logarithm rational. The implementation therefore does not put a misleading exact entropy value in the rational semantic core.

## Boundaries

This chapter does not claim:

- differential entropy for arbitrary continuous laws;
- invariant entropy under arbitrary changes of coordinates in continuous spaces;
- finite KL when supports are incompatible;
- information preservation by every learned representation;
- convergence of entropy-regularized learning;
- that a small KL step guarantees a performance improvement without the assumptions of a specific theorem.

The finite definitions are executable. Their continuous analogues require measure-theoretic care and often different invariance statements.

Continue to [Categorical learning](categorical-learning.md) for information objectives and gradients. Then read [Polarity, push-pull duality, and games](polarity-and-games.md) for forward state propagation and backward payoff evaluation.
