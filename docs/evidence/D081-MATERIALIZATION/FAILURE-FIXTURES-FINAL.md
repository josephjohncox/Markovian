# Complete replacement count/prefix failure literals

| case | e | W | H | R | Q | L | exact diagnostic |
|---|---:|---:|---:|---:|---:|---:|---|
| cap-M-not-min-E-A | 2 | 2816 | 528 | 152 | 680 | 23208 | ('old-elements', 5, 6) |
| pure-machine | 2 | 2816 | 528 | 152 | 680 | 23208 | ('machine', 9223372036854775808) |
| runtime-machine | 2 | 2816 | 528 | 152 | 680 | 23208 | ('machine', 9223372036854775808) |
| huge-old-cap | 2 | 2816 | 528 | 152 | 680 | 23208 | ('machine', 9223372036854775808) |
| old-elements | 2 | 2816 | 528 | 152 | 680 | 23208 | ('old-elements', 1152921504606846975, 1152921504606846976) |
| affine-elements | 2 | 2816 | 528 | 152 | 680 | 23208 | ('affine-elements', 1152921504606846975, 1152921504606846976) |
| pure-elements | 2 | 2816 | 528 | 152 | 680 | 23208 | ('affine-elements', 1152921504606846975, 1152921504606846976) |
| old-larger-competes | 2 | 2816 | 528 | 152 | 680 | 23208 | ('old-elements', 1152921504606846974, 1152921504606846975) |
| old-smaller-competes | 2 | 2816 | 528 | 152 | 680 | 23208 | ('old-elements', 1152921504606846973, 1152921504606846974) |
| scalar-pure | 1 | 2432 | 488 | 140 | 628 | 20084 | ('affine-elements', 0, 1) |
| scalar-runtime | 1 | 2432 | 488 | 140 | 628 | 20084 | ('affine-elements', 0, 1) |
| scalar-old | 1 | 2432 | 488 | 140 | 628 | 20084 | ('old-elements', 0, 1) |
| scalar-both | 1 | 2432 | 488 | 140 | 628 | 20084 | ('old-elements', 0, 1) |
| late-zero | 4 | 3584 | 608 | 176 | 784 | 29456 | ('success', 0) |
| first-zero | 4 | 3584 | 608 | 176 | 784 | 29456 | ('success', 0) |
| T-nil-uncredited-work | 1 | 2432 | 488 | 140 | 628 | 20084 | ('AffineWork', 896, 897) |
| scalar-nil-uncredited-startup | 0 | 2048 | 448 | 128 | 576 | 16960 | ('AffineConstructedCells', 4672, 4673) |
| first-debit | 0 | 2048 | 448 | 128 | 576 | 16960 | see source error order / uninspected rejected node |
| rank-axis0 | 1 | 2432 | 488 | 140 | 628 | 20084 | see source error order / uninspected rejected node |
| dimension-axis0 | 1 | 2432 | 488 | 140 | 628 | 20084 | see source error order / uninspected rejected node |
| rank-before-later-dimension | 2 | 2816 | 528 | 152 | 680 | 23208 | see source error order / uninspected rejected node |
| list-short-1-of-2 | 2 | 2816 | 528 | 152 | 680 | 23208 | see source error order / uninspected rejected node |
| list-excess-expected0 | 1 | 2432 | 488 | 140 | 628 | 20084 | see source error order / uninspected rejected node |
| transform-rank-b1-s1-r2 | 7 | 4736 | 728 | 212 | 940 | 38828 | see source error order / uninspected rejected node |

Fields tagged old-elements mean TensorShapeError(ElementLimitExceeded cap cap+1); affine-elements mean TensorAffineError(AffineLimitExceeded AffineElements cap cap+1); machine means TensorShapeError(MachineIndexOverflow M+1). Success rows are completed-header envelopes, not failed attempts. One-below exact prefix caps reject before the selected cons/nil; corresponding old/affine/machine diagnostics therefore do not occur. All these fixed failure envelopes include the independent F allowance, not a global retry charge.
