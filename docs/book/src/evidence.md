# Evidence records

The repository keeps durable command evidence for hardware-specific and proof-boundary claims.

## CUDA evidence

[CUDA evidence for 2026-08-26](https://github.com/josephjohncox/Markovian/blob/main/docs/evidence/CUDA-2026-08-26.md) records:

- The revision and host.
- The CUDA toolkit and driver.
- Differential test output.
- Transfer-inclusive benchmark samples.
- The warmup and sample policy.
- PTX regeneration commands.
- Historical measurements with explicit labels.

The measurement proves one local execution. It is not a general GPU performance claim.

## Acyclic open-system evidence

[S6 acyclic open-system evidence for 2026-08-27](https://github.com/josephjohncox/Markovian/blob/main/docs/evidence/S6-ACYCLIC-OPEN-2026-08-27.md) records:

- The accepted semantic scope.
- Package and compiler gates.
- Exact law fixtures.
- Rejection fixtures.
- Differential comparisons.
- Compile-fail proof boundaries.
- Deferred research claims.

Use an evidence record when a result depends on hardware, a benchmark method, or a large verification campaign.
