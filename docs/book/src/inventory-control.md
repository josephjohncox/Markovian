# Bounded serial-inventory benchmark

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
