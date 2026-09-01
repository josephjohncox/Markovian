# Bounded serial-inventory benchmark

This chapter covers the synthetic serial fixture, the Clark--Scarf finite specialization, and the Doğru balance comparison. The separate [fixed-batch chapter](fixed-batch-inventory.md) covers bounded `(R,nQ)` execution and stationary newsvendor evidence.

Markovian includes one **synthetic bounded serial fixture**. It is a small exact-control example. It is not a reproduction of a named published model. The primary-source equations and timing for those named families must be verified before the project uses their names for executable fixtures.

Use `Markovian.Benchmark.Inventory.Serial.Exact` to build and solve the model. Use `Markovian.Benchmark.Inventory.Report` to produce the deterministic comparison report.

## State, action, and event timing

A start-of-period state is:

```text
(t, u, a, i)
```

Here, `t` is the number of periods remaining, `u` is upstream on-hand inventory, `a` is the supplier order due at the start of this period, and `i` is downstream net inventory. Negative `i` is backlog.

An action is `(q, x)`. The supplier order and internal shipment satisfy:

```text
0 <= q <= orderCap
0 <= x <= u + a
```

One period has this order:

1. Receive `a` into available upstream inventory.
2. Choose supplier order `q` and downstream shipment `x`.
3. Ship `x` internally.
4. Observe demand `d`.
5. Charge holding or backlog cost against the successor inventories.
6. Record `q` as the supplier order due next period.

The transition equations are:

```text
u' = u + a - x
a' = q
i' = i + x - d
t' = t - 1
```

The one-period cost is:

```text
h0 * u'
+ (h0 + h1) * max(i', 0)
+ p * max(-i', 0)
```

The MDP transition reward is the negative of this cost. A state with `t = 0` is terminal, has zero terminal payoff, and exposes no action. The model does not clamp inventory or backlog states and does not redirect boundary transitions. Construction rejects more than 5,000 reachable states or 100,000 reachable state-action pairs. A solve rejects more than 100,000 target schedules. The module exports these budget values for inspection.

## Conditional bounded demand

Before conditioning, demand has the geometric law:

```text
P(D=d) = 2^-(d+1),  d >= 0
```

The fixture conditions this law on `0 <= d <= demandCap`. If `m` is the retained one-period mass, the report includes:

```text
retained one-period mass = m
omitted one-period mass = 1 - m
horizon exceedance probability = 1 - m^horizon
```

The last quantity is the probability that at least one independent draw from the original unbounded law exceeds the cap. It is **not** a value-error bound. Exact probabilities, returns, costs, and regrets apply only to the conditional bounded-demand model.

## Exact oracle and base-stock schedules

The oracle uses exact backward induction over `periodsRemaining`. The model accepts any exact finite-horizon discount in `[0,1]`, including `1`; contraction refinement is not part of this finite-horizon API. `solveSerialInventory` returns only a completed opaque solution.

A period-specific base-stock schedule gives targets `S0[t]` and `S1[t]`. Its action is:

```text
shipment =
  min (u + a)
      (max 0 (S1[t] - i))

order =
  min orderCap
      (max 0 (S0[t] - (u + a + i)))
```

The shipment clipping enforces physical availability. The order clipping enforces the supplier cap. Neither operation clamps the successor state.

The solver exhaustively evaluates every schedule in an explicit finite target grid. Grid construction rejects duplicate candidates and stores each period's set in canonical order, so exact ties do not depend on caller list order. The solution stores its initial state, grid, and model parameters. Costs, regret, and initial-state value error are derived from its two returns rather than stored redundantly.

A checked comparison requires the same initial state, horizon, discount, demand cap, and costs. It also requires a strictly larger order cap, a period-by-period candidate superset, at least one strictly wider candidate set, and completed solver status. Only then can unchanged exact values and nonbinding widened boundaries produce a stable result. A stable result is evidence for this small bounded fixture only.

## Deterministic report

`inventoryBenchmarkReport` accepts only the two opaque solutions and first validates their widening relationship. It records the complete primary and widened model parameters and target grids, solver method, timing, terminal and discount conventions, normalized demand probabilities, omitted mass, exact values, selected schedules, model sizes, solver status, and bound diagnostics. Its rendering has a golden test. It states the synthetic provenance and conditional-model exactness boundary directly.

## Reproducible timing executable

Run:

```sh
cabal bench inventory-control-bench --project-file=cabal.project.ci
```

The executable performs one complete warm-up and excludes it. It then performs twenty complete build, reachable-support construction, solve, and report samples. It prints every raw nanosecond sample and the arithmetic mean, sample standard deviation, minimum, and maximum. It also prints compiler, operating system, architecture, full model parameters, and model size.

Every timing sample must produce the same exact semantic report. A model, solver, report, or semantic-consistency failure stops the executable. These measurements document reproducibility on one toolchain and machine. They are not a performance comparison or scalability claim.

# Clark--Scarf 1960, Section III, finite lattice specialization

This named model is separate from the synthetic fixture above. The primary source is Andrew J. Clark and Herbert Scarf, “Optimal Policies for a Multi-Echelon Inventory Problem,” *Management Science* 6(4), 1960, pages 475--490, DOI `10.1287/mnsc.6.4.475`. The inspected scan has SHA-256 `b64d82098b47dffa7cc4b87a4bbc6c833bb90295ccbede0a1897c8af44956239`. Pages 481--482 define the Section III state, decisions, timing, and equations (11)--(15). Pages 483--484 give equations (20), (21), and (26) and Theorems 1--2. Arrow, Karlin, and Scarf, *Studies in the Mathematical Theory of Inventory and Production* (1958), Chapter 10, supplies the single-installation background cited by the article.

Use these modules:

- `Markovian.Benchmark.Inventory.ClarkScarf1960.Finite.Exact`
- `Markovian.Benchmark.Inventory.ClarkScarf1960.Oracle.Exact`
- `Markovian.Benchmark.Inventory.ClarkScarf1960.Report`

## State and event timing

The start-of-period state is `(x1,w1,x2)`. Here, `x1` is downstream net stock, `w1` is stock in transit to downstream, and `x2` is echelon-2 stock. The finite implementation also stores periods remaining. Construction checks:

\\[
x_2-x_1-w_1\geq 0.
\\]

An action selects external order `z` and downstream target `y`, subject to:

\\[
z\geq 0,
\qquad
x_1+w_1\leq y\leq x_2.
\\]

The finite implementation also checks `z <= orderCap`. The actual downstream dispatch is `y-x1-w1`. One period has this order:

1. Observe `(x1,w1,x2)`.
2. Select `z` and `y` at the beginning of the period.
3. Charge purchasing, transport, and beginning-stock holding costs.
4. Observe one demand `D` and charge shortage costs.
5. Pair that realized cost with the successor generated by the same `D`.

The successor is:

\\[
(x_1,w_1,x_2)'=
(x_1+w_1-D,\\;y-x_1-w_1,\\;x_2+z-D).
\\]

Old `w1` reaches downstream at the next state. The new dispatch remains in transit. The external order augments echelon 2 at the next state. This gives the two-period downstream and one-period upstream natural lead times described by the source.

For linear natural costs, the implementation uses the finite realized form of equation (1):

\\[
\ell(x,d)=h[x]^+ + p[d-x]^+.
\\]

Its exact demand expectation is `L(x)`. Keeping `ell(x,D)` and the successor in one transition outcome preserves correlation while reproducing the expected term in equation (14).

## Three exact paths

The checked finite equation-(14) recursion is:

\\[
C_n(x_1,w_1,x_2)=
\min_{\substack{0\leq z\leq z_{\max}\\x_1+w_1\leq y\leq x_2}}
\left\\{
 c(z)+c_1(y-x_1-w_1)+L_1(x_1)+L_2(x_2)
 +\alpha\\,\mathbb E\\,C_{n-1}(x_1+w_1-D,y-x_1-w_1,x_2+z-D)
\right\\},
\\]

with `C0=0`. The oracle compares this recursion with generic exact-MDP backward induction. It also computes the isolated equation (15), opportunity loss from equations (20)--(21), and upstream recursion (26). At every represented reachable state it requires:

\\[
C_n(x_1,w_1,x_2)=C_n^{(1)}(x_1,w_1)+g_n(x_2).
\\]

The policy reconstructed from the isolated target and equation (26) is evaluated in the original joint model. The checked fixture requires literal rational equality and zero regret.

## Computation and claim boundaries

The paper gives no numerical parameter table or published numeric oracle. The executable parameters are repository-authored. Replacing continuous integrals with finite rational sums, using an integer lattice, conditioning demand on finite support, and imposing order and isolated-target caps are computation decisions, not source claims. The report records retained mass, omitted mass, and horizon exceedance mass. These masses are not value-error bounds.

Reachability is complete within the validated finite model. There is no state clamping or successor redirection. State, state-action, and solver-work budgets are explicit. The report compares primary and strictly widened order and target caps and rejects unrelated evidence. An unchanged value with no widened boundary selected is a finite stability diagnostic, not proof of an unbounded minimizer.

The exact report says “Clark–Scarf (1960), Section III, finite lattice specialization.” It does not claim a published numeric reproduction, continuous-demand accuracy, average-cost convergence, a branching result, or a generic multi-echelon theorem.

Run the reproducibility executable with:

```sh
cabal bench clark-scarf-1960-bench --project-file=cabal.project.ci
```

It excludes one warm-up and measures twenty complete build, solve, and report runs. Every measured sample must return the same exact semantic report. Nanosecond values are local reproducibility data only.

# Doğru balance assumption: bounded finite-horizon adaptation

The Doğru benchmark is separate from both serial fixtures above. Its primary source is Mustafa Kemal Doğru, *Optimal Control of One-Warehouse Multi-Retailer Systems: An Assessment of the Balance Assumption* (2006), DOI `10.6100/IR601558`, Chapters 2--4. The implementation supports only two retailers, `l0=1`, and `l1=l2=0`.

Use:

- `Markovian.Benchmark.Inventory.Distribution.Dogru.Exact`
- `Markovian.Benchmark.Inventory.Distribution.Dogru.Report`

The inspected thesis crosswalk is: equations (2.3)--(2.5), printed page 24; equations (2.6)--(2.7), printed page 26; Table 4.1, page 95; Table 4.3, page 98; and the scenario-2 discussion and base stocks, pages 112--114.

The period order is: observe the state and known due order; choose the supplier order and retailer shipments; receive the due supplier order and the zero-lead retailer shipments; observe demand; then incur end-period cost. The repository stores local warehouse stock. Its coordinate map to the source state is:

\\[
w=I_0-IP_1-IP_2.
\\]

If `o` is the supplier order received this period, the physical shipments satisfy:

\\[
s_1\geq 0,\qquad s_2\geq 0,\qquad s_1+s_2\leq w+o.
\\]

The balance relaxation removes shipment nonnegativity. The implementation therefore uses a different signed action type and an explicit finite return cap:

\\[
s_i\geq-R,\qquad s_1+s_2\leq w+o.
\\]

One demand pair produces both cost and successor:

\\[
w'=w+o-s_1-s_2,\qquad o'=q,\qquad i'_j=i_j+s_j-d_j.
\\]

\\[
c=h_0w'+\sum_{j=1}^{2}\left((h_0+h_j)[i'_j]^++p_j[-i'_j]^+\right).
\\]

The two retailer demands are independent. Scenario 2 uses probabilities `78/100`, `7/100`, `7/100`, and `8/100` on `0`, `1`, `2`, and `3`; `h0=9/10`, `h1=h2=1/10`, and `p1=p2=4`. The thesis labels the scenario `cv_i=2`. That is a source label, not the exact coefficient of variation of this displayed finite law.

The exact comparison reports bounded relaxed oracle cost `C_R`, bounded physical oracle cost `C_P`, relaxed balanced base-stock policy cost, and physicalized LB heuristic cost `C_H`. It checks:

\\[
C_R\leq C_P\leq C_H.
\\]

Relaxation error `C_P-C_R`, policy regret `C_H-C_P`, and total gap `C_H-C_R` remain separate. Strictly wider order and signed-return caps must leave all four initial costs unchanged, and neither widened cap may be selected on the checked optimal paths.

This is a finite-horizon adaptation. It does not reproduce the thesis's published average costs. It does not use the thesis computation's lower state truncation or boundary redirection. Reachability is complete for the finite action system, and there is no state clamping. The benchmark excludes one warm-up and measures twenty complete runs. Timing is local reproducibility evidence, not a performance or convergence claim.
