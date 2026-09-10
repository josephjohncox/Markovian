## Exact fixture inputs and failure-state interpretation

The parameter convention is `(kind,b,s,r,B,V,n)`. Runtime accounts begin at4672/512/576 after legacy allocations; pure map construction does not import planning charges into a session.

* Signed map: base[5], view[3], offset4, strides[-2]. Runtime base has five finite values; contiguous seed[10,20,30] gives gradient[30,+0,20,+0,10].
* Overlap: base[4], view[2,2], offset0, strides[1,1]; first duplicate is logical pair(1,2), address1. Empty map: base[5], view[0], offset0, strides[0]. Scalar: []→[], offset0, strides[]. Singleton: [5]→[1], offset2, strides[0].
* Permute/reverse/slice extend the signed planning path, respectively permutation[0], reverse axis0, and target[3]/starts[0]/steps[1]. Each uses common transform s=r=1, B5,V3; actual scan counts differ.
* Bind uses the signed map. Signed pull adds pullback at n2. Empty pull uses base/view[0], offset0, strides[0], n2. Raw-empty failure: base[5], view[0,2], offset0, strides[0,7].
* Mixed1024 and batches[512,0,512] use the empty bind/pull geometry with exactly1024 already committed, spine-normal buffers; no hidden history normalization is assumed.
* Rank512 base means512 dimensions all1, count1, scalar view[], offset0, strides[]. The pull path has n2. Scalar-base/rank512 seed means base[], view512 dimensions all1,512 zero map strides, offset0, independent seed of that same rank/count, n2.
* Private bind failure can replace only the capacity field on an otherwise adequately backed five-scalar base with M div8+1. Count/capacity failure is before payload dereference. Private pull failure can replace only the independent three-scalar seed's dimension field with[M+37], retaining valid count/capacity/spine lengths; bounded seed-dimension diagnostic precedes allocation/read. These are admission/error fixtures, not permission for unsafe payload probes.

All seven nonempty subsets of the exact table's cells/work/live values lowered by one are in equations-FINAL.json with first operation/event, diagnostic and unchanged successful ledger. Do not interpret a semantic-failure row as a successful commit:

| semantic exit | successful ledger before and after |
|---|---|
|overlap or raw-empty pure failure|4672 /512 /576; caller's original immutable budget|
|private bind failure|4672 /512 /576|
|private pull failure after successful bind|50569 /6051 /1585|

The high-base path's bind Q67932 exceeds later pull Q42297. Therefore live-one-below, including competitions with eventual cells/work deficits, fails the earlier bind; no later pullback occurs.
