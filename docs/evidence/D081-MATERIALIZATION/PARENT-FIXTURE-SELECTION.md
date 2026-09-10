# Parent correction: controlling r4 fixture artifact

In response to supervisor request2e5af403-6b5a-4e3f-80ea-ae5edb143138, parent corrects the mistaken canonical selection in /tmp/d081-r4-review-brief.md.

Within /tmp/d081-proof-assembly-QQfz0BTQ, the controlling fixture document is FIXTURES-OPERATIVE.md, SHA256 8b9ae65b1fa93c517cb2734925103fb92ac01b18733a3a01d28e9a3bc0e78e99. This agrees with OPERATIVE-AMENDMENT.md §7, README selection, final equation generator and the assembled report. The controlling equation output is equations-FINAL.json, SHA256 23fe50e4138578a055c8af0d9cde0196e6cbf5e29bf33968d90f06f6a2290870. OPERATIVE-AMENDMENT.md SHA256 is270cb9a037a21fa4665e60b43d7c7ced3f90953489c33feb66ccb7da0f0fdc89.

FIXTURES-FINAL.md, SHA256 118e1df201d653aa98e07453530d57726667b59c7c12a858f848dee468f5a7b7, is NONOPERATIVE historical evidence despite its filename and equation citation. Do not repair or overwrite it. The parent independently compared the two tables: seven rows differ by +320 cells/+40 work/zero peak change; all other shared rows agree.

Controlling r4 cells/work/peak:
- signed-pull:123442/14894/2129
- empty-pull:112594/13538/2129
- mixed1024:316994/38066/10305
- batches512-0-512:316994/38066/10305
- rank512-base-scalar-pull-peak-before:6895157/848044/67932
- scalar-base-rank512-seed:10243125/1261740/104761
- pull-private-failure:123442/14894/2129

The review must explicitly record the parent's brief-selection error and historical table discrepancy. This is an artifact-selection correction, not a change to r4 source, equations, coefficients or campaign bytes. No rerun or mutation of the retained campaign is authorized. Any later normative repository documentation must use the operative set and must not reproduce the historical table as current.

Continue independent review on this controlling set. DESIGN PASS, if independently justified, is not itself refreeze, runtime resumption, acceptance or release authority.
