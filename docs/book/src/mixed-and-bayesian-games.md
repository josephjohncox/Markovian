# Exact mixed, stochastic, and Bayesian games

Markovian's exact strategic layer verifies finite represented candidates. It does not solve every finite game and does not assert equilibrium existence.

## Modules

- `Markovian.Game.Profile.Finite` owns checked heterogeneous owner products, pure profiles, and complete exact simplexes.
- `Markovian.Game.NormalForm.Exact` owns rational payoff tables, independent mixed profiles, expectation, pure deviations, and mixed-Nash checks.
- `Markovian.Game.Correlated.Exact` owns joint correlation devices and separate CE and CCE reports.
- `Markovian.Game.Outcome.Exact` owns complete joint outcome laws.
- `Markovian.Game.Stochastic.Exact` evaluates finite-horizon public-state Markov profiles and verifies finite-horizon Markov-perfect candidates.
- `Markovian.Game.Harsanyi.Exact` owns correlated common priors, behavioral type policies, Bayes-Nash checks, and bounded strategic-normal conversion.
- `Markovian.Game.Open.Strategic.Exact` extracts one checked closed context from a pure finite open game.

All these modules use `Rational` and depend only on `base` and the root finite witnesses. Constructors are opaque and nominally role-protected.

## Owned products and simplexes

An `OwnedProduct owner choice` contains one nonempty finite choice carrier per owner. Product cardinality and owner/profile cell counts are checked before profile generation. Profiles use canonical owner-major order.

An `ExactSimplex` is a complete duplicate-free table. Every mass must be nonnegative, and masses must sum literally to one. The constructor does not normalize, merge labels, or repair a zero total. Positive support is derived from the complete table.

`GameLimits` bounds owners, local carriers, product profiles, cells, represented work, rational numerator/denominator size, and dynamic horizon. Every consuming game operation revalidates stored rational values against its active limits. Products, sums, contingent-plan powers, cells, and report work use capped preflight before result enumeration. A rational size check occurs after each represented arithmetic step; it is not a transactional heap guarantee. Any operation failure returns no semantic result or partial report.

## Independent mixed Nash

For an independently mixed profile `sigma`, expected utility is

\\[
U_i(\sigma)=\sum_a\left(\prod_j\sigma_j(a_j)\right)u_i(a).
\\]

For every owner and pure action, `checkMixedNash` computes the exact unilateral value while retaining all opponents' rows. Checking pure deviations is sufficient for a supplied candidate because unilateral expected utility is affine in the deviating simplex. This verifies against real mixed deviations; it is not merely a rational-grid check.

The checker is not an equilibrium finder. It supplies no existence theorem. A three-player rational-payoff fixture gives action `B` payoff zero and action `A` payoff `1/2` only when both opponents choose `A`, and `-1/2` otherwise. Symmetric indifference requires

\\[
p^2-\frac12=0,
\\]

so `p=1/sqrt(2)` is not representable by `Rational`. A zero-payoff game is the separate degeneracy counterexample: every mixed profile is an equilibrium, so support systems can be positive-dimensional. A future exact solver must classify such systems instead of silently skipping singular cases.

The test differential enumerates every two-player `2 x 2` payoff table over `{0,1}` and candidate probabilities `{0,1/2,1}`. It compares the public checker with a separately written direct formula.

## CE and CCE

An `ExactCorrelationDevice` is one joint distribution over pure profiles. It is distinct from an independent mixed profile.

CE uses each direct-recommendation inequality

\\[
\sum_{a_{-i}}\mu(a_i,a_{-i})
 [u_i(a_i,a_{-i})-u_i(b_i,a_{-i})]\ge 0.
\\]

The implementation does not divide by recommendation mass. A zero-mass recommendation has zero unconditional slack and status `NullRecommendation`; it is not described as conditionally optimal.

CCE instead checks every constant pre-recommendation deviation

\\[
\sum_a\mu(a)[u_i(a)-u_i(b_i,a_{-i})]\ge 0.
\\]

The types and reports remain separate. Fixtures show:

- every enumerated product mixed-Nash candidate in the binary differential passes CE;
- for every pure profile in every binary payoff table, its Dirac device is CE exactly when the profile is pure Nash;
- independence rejects different owned carriers, including a device that merely deletes zero-mass actions and a reordered layout;
- the exact half-half matching-pennies Nash product passes CE;
- half mass on each coordinated Battle of the Sexes outcome is not the product of its uniform marginals;
- mass `1/2` on `(A,L)` and `(C,R)` in the documented three-row fixture passes CCE but fails CE;
- degenerate and irrational boundaries prevent a general rational enumeration claim.

No CE-polytope, LP, LCP, Lemke--Howson, support-enumeration, or denominator-grid solver is included.

## Joint stochastic outcomes and timing

`ExactStageOutcome` stores one reward vector and one successor state. `ExactOutcomeLaw` keeps complete atoms. Separate reward and successor marginals are not accepted by `exactStochasticGame`. For this additive risk-neutral evaluator, `E[r + gamma V(s')] = E[r] + gamma E[V(s')]`, so the two marginals determine the represented expected value. The joint law is retained for pathwise composition, post-transition observations, and future nonlinear or risk-sensitive semantics; no stronger correlation claim is made.

For remaining horizon `h` and public state `s`, evaluation uses this order:

1. Check terminal status.
2. If terminal, return terminal value once and request no action or transition.
3. If `h=0`, return zero additional value.
4. Expose the current public state.
5. Apply the local independent mixed profile.
6. Use one joint reward/successor atom.
7. Add transition reward once.
8. Decrease horizon once and discount continuation once.

The exact recurrence for nonterminal states is

\\[
V_i^h(s)=\sum_a\prod_j\sigma_j^h(a_j\mid s)
\sum_{(r,s')}K(r,s'\mid s,a)
 [r_i+\gamma V_i^{h-1}(s')].
\\]

A Markov-perfect candidate check builds this continuation normal form at every positive horizon and nonterminal state, then runs the mixed-Nash checker. The scope is finite horizon, public state, perfect monitoring, simultaneous actions, and independent private randomization. There is no sampled interpreter, generator, stationary solver, private observation history, correlated Markov policy, or infinite-horizon existence claim.

## Harsanyi games

An `ExactTypePrior` is a common prior over complete type profiles. It can correlate owners' types and is never reconstructed from marginal products. An `ExactBehaviorProfile` gives one action simplex for every owner and own type. The initial bounded fragment uses one action carrier per owner across all its types.

For owner `i`, own type `t_i`, and pure action `b_i`, the checker compares exact unnormalized sums. If the marginal `P(t_i)` is positive, the report also includes conditional values. If it is zero, conditional fields are `Nothing` and status is `NullPriorType`. The API does not invent an off-path posterior.

`checkPositiveTypeInterimBayesNash` and `checkExAnteBayesNash` have distinct report types. In this one-shot perfect-recall fragment, a contingent-plan deviation separates by own type. `toStrategicNormalForm` independently enumerates complete contingent plans after preflighting capped powers, profile products, cells, and work. It is not agent normal form because owner types are not split into independent agents. The correlated-prior fixtures compare direct behavioral values with converted normal-form payoffs.

This does not provide type-dependent action carriers, caller-supplied null-type beliefs, repeated or extensive games, imperfect recall, refinements, Bayes-correlated equilibrium, or general disintegration.

## Checked open-context extraction

A generic `FiniteOpenGame` cannot be mixed safely. Its whole profile carrier does not expose owner-local replacement and its best-response callback is Boolean rather than a numeric utility.

`normalFormFromOpenContext` therefore requires an opaque `OwnedDeviationLayout`: a complete bijection between an explicit owner-local product and the represented global strategy carrier. For one checked context and one exact owner utility projection, extraction computes every pure payoff and exhaustively compares the existing callback with exact unilateral maximization. A mismatch is an error.

The result is one contextual normal form. It is not a compositional probabilistic open-game semantics, strategic duality, Bayesian inversion, payoff pullback, matrix dagger, reverse differentiation, feedback, or disintegration.

## Example, evidence, and references

`MixedGamesExample` builds matching pennies, verifies the exact half-half mixed-Nash candidate, converts its product masses to a separate correlation device, and verifies CE. It is a verifier example, not an equilibrium solver.

`test/MixedBayesianGames.hs` contains construction, laws, independent enumeration, Dirac-CE/pure-Nash and product-Nash/CE differentials, carrier mismatch, timing, null-type, correlated-prior-sensitive conversion, degeneracy, active-rational-limit, contingent-plan one-below, report-golden, and atomic-budget fixtures. `scripts/check-mixed-game-boundary` checks opacity, nominal roles, removal of the superseded agent-normal name, and separation of priors, independent profiles, and correlation devices. `mixed-games-exact-bench` uses one excluded warmup and twenty samples, reports host/compiler/clock metadata and raw CPU-time samples, and asserts a pinned FNV-1a checksum of its deterministic semantic report.

The general `exactFiniteDist` constructor rejects raw supports above 4096 entries and consumes at most one additional list cell. General sequencing is available only through `bindExactFiniteDistChecked`, with explicit result-support, work, numerator-bit, and denominator-bit limits. The game layer retains its separate `GameLimits`; it does not borrow general distribution limits or use checked bind to bypass game-specific admission. D-061 is `Accepted` after its topology, compiler, documentation, archive, and hosted gates passed.

Primary sources:

- Nash, “Non-Cooperative Games,” 1951, DOI `10.2307/1969529`.
- Aumann, “Subjectivity and Correlation in Randomized Strategies,” 1974, DOI `10.1016/0304-4068(74)90037-8`.
- Aumann, “Correlated Equilibrium as an Expression of Bayesian Rationality,” 1987, DOI `10.2307/1911154`.
- Harsanyi, “Games with Incomplete Information Played by Bayesian Players,” Parts I–III, DOIs `10.1287/mnsc.14.3.159`, `10.1287/mnsc.14.5.320`, and `10.1287/mnsc.14.7.486`.
- Shapley, “Stochastic Games,” 1953, DOI `10.1073/pnas.39.10.1095`.
- Fink, “Equilibrium in a Stochastic n-Person Game,” 1964, DOI `10.32917/hmj/1206139508`.
- Lemke and Howson, “Equilibrium Points of Bimatrix Games,” 1964, DOI `10.1137/0112033`.
- Ghani, Hedges, Winschel, and Zahn, “Compositional Game Theory,” 2018, DOI `10.1145/3209108.3209165`.
