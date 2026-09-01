# Finite open games and pure equilibria

Markovian implements a bounded deterministic fragment of open games. It is separate from both structured-cospan topology under `Markovian.Open.*` and alternating protocol histories under `Markovian.Game.Arena`.

The public modules are:

- `Markovian.Game.Optic.Finite` for finite functions, bijections, and concrete optics;
- `Markovian.Game.Open.Finite` for owned open games, composition, equality, and bounded pure-equilibrium enumeration;
- `Markovian.Game.Open.Exact` for exact rational maximizing decisions, exact-payoff contexts, and deterministic reports.

## Concrete finite optics

A `FiniteOptic x s y r` is only the checked pair

\\[
P:X\longrightarrow Y
\\]

and

\\[
C:X\times R\longrightarrow S.
\\]

Both functions are total tables on explicit finite carriers. Labelled extensional equality is separate from represented-layout equality.

For composable optics `G : (X,S) -> (Y,R)` and `H : (Y,R) -> (Z,Q)`, the implementation checks

\\[
P_{H\circ G}(x)=P_H(P_G(x))
\\]

and

\\[
C_{H\circ G}(x,q)=C_G\left(x,C_H(P_G(x),q)\right).
\\]

This is the concrete cartesian shape compared with Riley, section 2. Markovian does not implement arbitrary residual objects or the general coend optic construction.

## Open-game data

A `FiniteOpenGame owner strategy x s y r` stores:

- a finite strategy-profile layout;
- a structural ownership tree whose leaves pair owners with local strategy carriers;
- total play `strategy x X -> Y`;
- total coplay `strategy x X x R -> S`;
- best-response membership indexed by a context `(x,k)` with `k : Y -> R`.

A strategy schema is either the owner-free singleton unit, one owner with a finite local strategy carrier, or an internally composed product. Composition rejects repeated owners. The rejected case would require one owner to coordinate a deviation across several sites, which this fragment does not implement.

A context is checked against the game's labelled boundaries. Its continuation table is canonicalized to the game's target and utility layouts before a best-response callback can inspect it. The exact decision constructor enumerates all represented functions `X -> Y`. Its strategy count is

\\[
|\Sigma|=|Y|^{|X|}.
\\]

The implementation uses the standard finite convention `0^0=1`. It computes cardinalities with `Natural` before allocation and rejects insufficient bounds.

## Sequential composition

For

```text
G : (X,S) -> (Y,R)
H : (Y,R) -> (Z,Q)
```

the composite profile is `(sigma,tau)`. Play and coplay are

\\[
P_{H\circ G}((\sigma,\tau),x)
=P_H(\tau,P_G(\sigma,x))
\\]

and

\\[
C_{H\circ G}((\sigma,\tau),x,q)
=C_G\left(\sigma,x,C_H(\tau,P_G(\sigma,x),q)\right).
\\]

The upstream continuation uses the incumbent downstream strategy:

\\[
k'(y)=C_H\left(\tau,y,k(P_H(\tau,y))\right).
\\]

Then the represented response pair belongs to the composite relation exactly when

\\[
(\sigma,\sigma')\in B_G(x,k')
\\]

and

\\[
(\tau,\tau')\in B_H(P_G(\sigma,x),k).
\\]

The incumbent in these formulas matters. The sequential fixture includes an entry-deterrence game whose pure Nash set contains a non-credible threat. This is a counterexample to interpreting the enumerator as a subgame-perfect solver.

## Tensor composition

Tensor uses product play and coplay. Each local response holds the other incumbent play fixed:

\\[
k_1(y_1)=\pi_1\left(k(y_1,P_2(\sigma_2,x_2))\right)
\\]

and

\\[
k_2(y_2)=\pi_2\left(k(P_1(\sigma_1,x_1),y_2)\right).
\\]

Owner overlap is rejected for tensor as well as sequential composition.

## Pure contextual equilibrium

A represented profile is a pure equilibrium in context `(x,k)` when

\\[
(\sigma,\sigma)\in B(x,k).
\\]

`enumeratePureEquilibria` returns every such profile in represented layout order. Exact decisions compare `Rational` values literally. Ties retain all maximizers.

Fixtures cover prisoner's dilemma, coordination, exact ties, and matching pennies. Matching pennies has no pure equilibrium. This does not contradict Nash's 1951 finite mixed-equilibrium theorem: mixed strategies are not implemented here.

The differential fixture exhausts all two-player `2 x 2` payoff tables whose two utility tables use `{0,1}`. For each table, open-game equilibrium enumeration agrees with a separate unilateral-deviation enumeration.

## Observational equality

`observationallyEqualUnder` receives an opaque structural strategy bijection. Same-owner leaf relayouts, products, units, associators, and symmetries are explicit; an arbitrary whole-profile permutation cannot exchange owner-controlled leaves. Before allocation it bounds the work needed to compare:

- play for every profile and input;
- coplay for every profile, input, and utility;
- best-response membership for every input, represented continuation, and profile pair;
- owner support.

Two games with equal play and coplay but different best-response relations are therefore unequal. Every advertised callback is forced, actual performed counters are retained, and completed equilibrium result spines are forced. The comparison is exhaustive only for the supplied finite carriers. It is stricter than the strategy-bijection quotient in Ghani and colleagues because the complete ownership structure must agree.

There is no `Category` instance. Identity and representative associativity are checked only through explicit finite bijections. These fixtures are not universal Haskell proofs.

## Bounds and ownership

Every framework-owned table traversal or enumeration has an explicit limit. Composition computes profile, boundary, table, and continuation cardinalities with `Natural` and rejects oversized products before materialization. Budget exhaustion returns an error and no partial game, function space, equilibrium set, or equality result. Reports contain deterministic performed counts and exact values, but no timing fields.

The caller-supplied best-response evaluator owns its internal termination and resource use. A framework bound cannot make an arbitrary callback constant time, and it does not bound the cost of arbitrary-size `Rational` arithmetic.

## Boundaries

This fragment does not establish:

- equilibrium existence;
- mixed, correlated, repeated, or subgame-perfect equilibrium;
- stochastic, Bayesian, incomplete-information, or continuous games;
- chance as a player;
- arena legality, justification, views, innocence, or bracketing;
- universal categorical or optic laws;
- literal associativity of nested strategy products;
- one owner controlling several composed decision sites;
- disintegration or Bayesian inversion;
- integration with MDP rewards or successor kernels.

The implementation follows the finite pure-strategy formulas in Ghani, Hedges, Winschel, and Zahn. Definition 3 supplies open-game data, Definition 4 supplies the maximizing decision specialized by `exactMaximizingDecision`, and Definitions 9, 10, and 12 supply composition, strategy-bijection equivalence, and tensor. Escardó and Oliva supplies the comparison with stronger sequential optimality. See [References and further reading](references.md#ghani-and-colleagues-compositional-games).
