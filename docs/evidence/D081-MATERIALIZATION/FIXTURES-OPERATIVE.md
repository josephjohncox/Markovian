# Independently derived replacement fixtures

| Fixture | cells | work | peak |
|---|---:|---:|---:|
| signed-map | 68255 | 8308 | 1215 |
| overlap-post-admission | 104142 | 12778 | 1342 |
| empty-map | 49535 | 5968 | 1215 |
| scalar-map | 38332 | 4590 | 1036 |
| singleton-map | 51967 | 6272 | 1215 |
| permute | 150153 | 18365 | 1442 |
| reverse | 150153 | 18365 | 1442 |
| slice | 150153 | 18365 | 1442 |
| bind | 50569 | 6051 | 1585 |
| signed-pull | 123442 | 14894 | 2129 |
| empty-pull | 112594 | 13538 | 2129 |
| raw-empty-post-admission | 59278 | 7170 | 1342 |
| mixed1024 | 316994 | 38066 | 10305 |
| batches512-0-512 | 316994 | 38066 | 10305 |
| rank512-base-scalar-bind | 3593756 | 440656 | 67932 |
| rank512-base-scalar-pull-peak-before | 6895157 | 848044 | 67932 |
| scalar-base-rank512-seed | 10243125 | 1261740 | 104761 |
| bind-private-failure | 50569 | 6051 | 1585 |
| pull-private-failure | 123442 | 14894 | 2129 |

All seven nonempty subsets of individually one-below limits are checked in equations-FINAL.json. Within each admission event, failure order is cells, then work, then live; an earlier operation live shortage can precede a later cumulative cells/work shortage. Its operation/stage and unchanged ledger are recorded; a live cutoff need not occur in the last operation. Semantic-failure rows are admission thresholds, not successful ledger commits. See the operative amendment for old-payload and semantic precedence.
