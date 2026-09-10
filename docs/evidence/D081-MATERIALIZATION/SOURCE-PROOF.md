# Source → elementary schedules → operative r4 proof

This is an integrator's logical-source proof, not a compiler allocation measurement. Read with OPERATIVE-AMENDMENT.md. The complete original advisory expansions are preserved verbatim in `advisories/d081-{prefix,geometry,runtime}-proof.md`; their source citations and primitive summands are the audit trail, not authority. The substitutions and ownership partition below override their smaller/disagreeing schedules. No new unit convention is selected.

## 1. Units and what was reconciled

All three lanes use frozen §6.1/§8.1 slots/primitives. Their *conservative expansions* differ: B deliberately reads/assigns/inspects extra argument and closure references, while A/C sometimes forward already-held registers directly. Reading a local register is not another recursive field/dictionary traversal. A's arithmetic-plus-explicit-result assignment2 and B's arithmetic1 plus separately listed demand/control are upper schedules in the same primitive unit, not different CPU units. B's T(a)=4a+12 is a sum of argument reads/assignments, entry/return, captured-field assignments and inspections; it is not a mandatory indivisible call or a new allocation primitive. Tail calls whose result is returned directly have no source reference to a history of prior results. Their logical active register frame can be reused; this asserts nothing about GHC stack/heap retention. Strict non-tail result construction has explicitly accumulating saved frames.

At most eight slots per expanded primitive: inspection/branch≤2; bounded scalar result≤2; widened arithmetic/current result≤4; dispatch/return≤8 **after all saved fields/arguments have their own assignment steps**; record/list/closure/memo installation one assignment per field. A whole natVal/knownShape/debit/quotRem/traversal is never one step. Large supplied Nat evidence is projected as an old referent/numeric view; comparisons against bounded caps do not copy that referent or execute arbitrary user instances. This is not a bit-complexity/physical representation theorem.

Corrections after actual I/S/library reads:

1. Import B's larger shared geometry helper expansions into C by substitution, not addition. Re-reading natVal in affineDimensions uses A's11 plus bounded conversion2 instead of B's compressed four extraction primitives:30−4+13=39 per axis.
2. B's two-slot numeric lists count only link/element slots. Reserve two additional slots per potentially new numeric referent: numeric lists/digits4r; zipper pairs/list5r still share their element referents. Add six potential scalar slots for final offset/low/high. No source assignment is constructed twice when a numeric referent is shared.
3. Include prepareSessionCommit's pre-header work and frames in the common fixed pool. Remove the duplicate publication/rollback portions from C. Fixed source work1872≤2048 and storage420≤448 demonstrate the fit; **no body materializer borrows the remaining fixed slack**.
4. Installed modern library source confirms the actual scanr tail captures f as well as q0/xs. Its tail recipe is four slots, not three. Tag+branch+fields are four operations, not the abbreviated three. Per-axis work37≤40 remains, retained allowance rises26→27. Pending bounded `toInteger dimension` operands require an independent2r transient allowance during suffix multiplication; they are not hidden in the frame or final-product reservation.
5. Price a post-admission coordinate diagnostic once in addition to finite cleanup:82≤88;88+39=127≤128. Do not recycle unused F or success result work to justify that diagnostic.
6. Failure records can retain bounded numeric referents as well as their reference fields: maximum17, not nine. Full R dominates that bound without returning a list spine or parent history.
7. Ownership association can finish an owner record whose strict SShape field has only a pending outer stock constructor. It forces at most that outer constructor, never the owner witness tail. The common fixed pool separately prices this optional14-slot ownership packet; the tensor item's P excludes that packet.

Provisional executed sources r1–r3 and their passing formula controls remain intact. r1 overreserved transaction work/storage separately; r2 assigns it once to the fixed pool; r3 separates scanr references/operands; **r4** explicitly prepays the finite kernel diagnostic. Only r4 is operative. Model r3's failed global competition assertion is preserved; model r4 corrects cross-operation timing; FINAL includes the final source/storage/diagnostic reconciliation.

## 2. A: fixed and per-inspection proof

I402–545; S25–50. A's arithmetic bodies are expanded as: satAdd successful/overflow worst13+call4=17; satMul worst19+call4=23 (zero first); credit success11/failure21; oldCheck success22/failure28; inline old dimension check22. Constructor alternatives include tag inspection/branch, cap projection, bounded cap+1, error fields/wrappers and Either control. No rejected large product is formed.

The successful prospective debit is5 header inspection/projections +3×17 additions +4 cap/peak accesses +4 max +3×11 successful credits +9 binds +6 header/Right slots +5 call =117≤128. Failed cells/work/live paths are52/84/121, all≤128, and inspect no node. The one failed debit is in F.

Stock knownShape cons: evidence selection2, method dispatch/return3, recursive tail recipe2, proxy control1, SCons fields/tag4, memo update1, producer selection2 =15 work. Potential survivors8. Stock nil≤4 work/two slots. Dimension evidence projection11 work and≤3 new view/memo slots. Thus per inspection stock/evidence survivors≤8+3=11≤12. Repeated/old witnesses and raw nodes can use less; their coupon is unchanged. A cons's suspended tail recipe is included, not production of the next constructor.

Shape-cons non-debit sum:5 match/fields +15 stock +11 evidence +33 old rank +27 affine rank +25 old dimension +34 affine dimension +2 conversion +4 zero +25 product +15 recursion/propagation +4 strict registers +6 continuation =206. Add debit117 →323≤384. Shape nil:4 producer+4 registers+2 case+31 old elements+25 affine elements+5 machine guard+8 diagnostic+6 propagation+117 debit=202≤384. Raw-list uniform bound117+4+3+13+13+2=152≤384. These sums include alternative failure maxima rather than counting an entire helper as one step. Semantic early exits execute prefixes of the schedule; element/machine failure occurs only at charged nil.

Transient slots per admitted inspection: debit continuation/frame8 + scanner argument/frame7 + stock producer activation4 + return shell2 =21; add independently represented seen/product/dimension/candidate-meter scalars12 and retained completed header record4 →37≤40. Those six bounded scalar values are not smuggled into reference slots. Fixed old/current candidate registers belong to the active pool; finished headers move to the per-inspection pool. Any survivor transfer removes the identity from actual transient storage, although the conservative transient capacity is retained in the bound.

### Fixed source audit against the replacement formula

| Fixed family | work |
|---|---:|
|policy/setter/budget/usage|227+24+20+6=277|
|runtime closed/enabled/counter entry|67|
|old payload preflight|4×24+3×17+7 field accesses+7 call+4 pull argument arithmetic=165|
|replacement formula, maximum transform branch|16×17+17×23+49=712|
|full reservation|2×17+9 fields+3×21 credit maxima+9 binds+4 max+6 usage/Right+5 call=130|
|one rejected debit|128|
|fixed scan starts/rank/result orchestration|28+9+60+34+33=164|
|fixed terminal forwarding only|6×3=18|
|transaction source schedule below|117|
|first machine-maximum view/conversion/assignment/control|6|
|runtime callback/private-prepare forwarding, detailed below|48|
|optional ownership/outer-witness completion, detailed below|40|
|**sum / rounding**|**1872≤2048**|

The formula saturated add/multiply counts, respectively, are map12/12, transform16/17, bind8/7, pull14/13. The additional49 consists of A's original38 non-arithmetic controls/records plus8 fields of the three lazy common recipes (pairs2, addresses3, collisions3) and three demands. Only the relevant branch demands those recipes. Numeric polynomials are pre-expanded; no runtime term-list traversal is introduced. The pair factor is divided before multiplication. The source and independent checker count at most33 arithmetic applications. The formula storage208 is an aggregate, not a claim that every call's entire graph is six slots: at most33 three-field application recipes99, at most nine active two-argument frames at B's nine-slot frame ceiling81,18 slots for saved polynomial/memo results, and10 for the profile tuple/fixed selectors. The deepest saturated chain is transform's six nested work additions plus one term multiplication, under cells' outer addition and multiplication:9; live/workspace/retained and pair/address/collision demands are shallower. Pending arithmetic values use the separate28-slot active scalar arena (at most eight completed left operands16 plus active widened/current values≤8, rounded24≤28). The18 saved-result slots cover work/workspace/retained/live/cells, pairs/addresses/collisions and the intermediate8work, each at most two slots. Shared result identities later installed in charge/usage transfer into the reserved survivor capacity rather than being constructed again.

Transaction I1453–1457: T(2)20, mask closure5, mask controls5, take administration6, three binds9, restore-handler closure5, prepare dispatch T(1)16 (callee separate), evaluate/pair elimination4, handler installation2, publish-or-restore administrations conservatively2×21, delivery3: total117. Callee/allocator work is not hidden there. Frames/closures/results:9+3+1+3+3+4+9+2+3×5=49. This executes around the header, so it is not postponed to a post-admission body budget. Separately, the operation-created IO callback invokes its private pure `prepare`: dispatch T(1)16, private closure assignment/inspection8, case2, failed Left reconstruction2, candidate pair3, pure dispatch3 total34; the outer callback closure adds at most7 (four fields plus three reference inspections), giving41≤48 work. The successful forwarding branch is smaller. Additional simultaneous slots are private closure4+private-prepare frame7+old Left shell2+outer callback closure4=17; the candidate pair is already in the transaction's49, not counted again.

Fixed transient inventory:16 entry registers +20 constant scanner closures +12 active headers +13 meter/cap registers +28 scalar scratch +(33×3+9×9+18+10) formula slots +12 diagnostic preparation +24 outer controls +8 lazy formula closures +49 transaction +17 operation callback/private-prepare +13 optional ownership completion =420≤448. These are distinct arenas; sequential phases are deliberately summed. None of C's descriptor, producer, coordinate or registry slots uses the spare28.

Fixed potential survivors:43 header/handoff/charge/usage reference slots (18+16+5+4),14 numeric referents for charge/usage, up to36 numeric slots attached to the18 bounded fact references, and6 for three newly completed bounded numeric CAFs =99≤128. Charge/usage nine record slots are constructed in A and shared into body reports, not reconstructed there. Pre-body diagnostics, including a possible global machine error, fit the same128 bound (≤99+17). On a body failure its packet belongs to the body's result/diagnostic pool, not a second A packet.

I1738–1767 ownership completion is fixed but not implicitly free: OwnedTensor3 + TensorOwner3 + one newly demanded stock SCons/proxy/tail/memo8 =14 possible survivors. This is separate from the underlying tensor-item packaging. Work15 stock +6 record assignments +4 inspections +4 association call +5 bounded selection/forwarding =34≤40. Activation/control13=stock4+association frame7+return shell2. An observed successful tensorOwner decision already established nonempty String; no String rendering or witness-tail/natVal traversal is introduced. Completed owners cost less. These objects are demanded only after full admission: body common survivors≤99+14=113≤128, while pre-body diagnostics have no owner completion and≤99+17=116≤128. Do not sum the mutually exclusive pre-body diagnostic and post-body owner alternatives. Bind materializes at most one supplied ownership packet; a completed binding has already discharged that packet for pullback. Arbitrary pending caller ownership pipelines remain outside the completed-producer boundary.

Startup source227+24+20+6=277 plus session-entry shell38 (state8, session4, MVar controls4, closure4, setup8, handler/branch8, demands2)≤48 gives325≤512. Its objects are below the same448/128 arenas; no close/finalizer history is priced as startup. Hence L0=8×512+448+128=4672. These are independent sufficient bounds, not measured exact costs.

Thus WA=2048+384e, HA=448+40e, RA=128+12e; Q=576+52e and L=16960+(8×384+52)e=16960+3124e. The fixed failure allowance is independent of path credit, exactly as the original failure contract permits.

## 3. B: pure geometry and kernel

The full source-cited per-helper summands and iteration terminals are in the captured geometry appendix. The controlling helper table after reconciliation is:

| helper | work ceiling |
|---|---:|
|affineDimensions|18+39r|
|affineSignedInput / affineSignedResult|29 /34|
|affineSignedInputs|48+58r|
|affineRawEmpty|43+22r|
|affineIntegerInputs|18+27r|
|affineNormalize|28+42r|
|affineZipper|49+42r|
|affineExtrema|71+164r|
|affineContiguous|44+40r|
|affineAddress direct|104+96r|
|validator address adapter|20 additional per call|

Digits: terminal26 plus45r, explicitly including two quotRem scalar operations, tuple5 work, field inspections2 and digit-cons4. Dot: terminal41 plus51r, including both signed term/partial-address checks, conversions, arithmetic and both spines. Wrapper37 yields104+96r. The kernel workspace is15 kernel frame+11 digit frame+13 dot frame+20 numeric temporaries+3 quotient/remainder tuple+2 captured-bound closure=64, plus4r digit list/numeric slots. No second simultaneous digit array exists.

Validator nonempty sum is304+246r+V(208+96r)+P(284+192r); empty costs44 and skips extrema/zipper/kernel/pairs/contiguity. It invokes V+2P kernels, not V: the first address is recomputed in every pair. There are V ordinary address iterations, V outer-pair iterations and **V separate suffix terminals**, P suffix iterations, and r(V+2P) digit and dot steps. Pair order is lexicographic (0,1),(0,2),…,(V−2,V−1). Failure truncates that schedule. The largest bounded body diagnostic schedule is9 assignments+9 inspections+3 return controls+T(1)16+sentinel/demand2=39, once per body.

Transform helpers (all nils/early exits included): axis range29; pick54+31s; permutation ranges25+56s; duplicate suffixes42+64s+36D where D=s(s−1)/2; permutation dimensions59+r(94+31s); selected strides25+r(94+31s); copy18+26r; reverse total40+36r, not a fresh full copy per selected axis. Nonzero steps42+28s; domains72+152s; slice offset60+113s; slice strides52+85s. Ranges precede duplicate pairs; lookup visits at most s per requested axis, so two families≤2rs, not cubic. Reverse does two lookups only for nonempty output. Slice performs all signed starts, all signed steps, nonzero checks, all domains, then nonempty offset/stride derivation; it is linear, no slice-specific s² term needed.

B's original helper sums become new670+434r+J (one9r evidence correction); permute756+199s+515r+18s²+62sr+J (9s+9r); reverse826+31s+394r+J (9r); slice889+533s+327r+J (9s+9r). For s=r these are756+714s+80s²+J,826+425s+J,889+860s+J. Coefficientwise maximum yields889+860s+80s²+J. These sums include one39 diagnostic and6 signed-constant work, not another global F.

Finish non-validator work122: activation64, map24, budget6, report6, pending continuation10, Right3, bind/projections6, strict checks3. The fixed new result/reference packet has25 slots (map12+budget3+report3+continuation5+Right2). Add potential offset/low/high numeric referents6 and normalized strides/list-values4r →RB31+4r. The packet shares A's already-created charge/usage and their numbers through newly charged report references. No derived map retains a parent-map/history field.

B fixed simultaneous storage is273:20+31+15+15+15+11+13+9+9+9+11+7+9+56+19+24. This includes the separate ordinary-address, pair-outer and pair-suffix loop frames. Numeric56=18 derivation+4 extrema results+6 contiguity+20 kernel+8 pair. Intermediate24=5 extrema pair/Either+6 validator triple/Either+10 helper Eithers+3 quotRem tuple. The potential diagnostic area is a result alternative; shared identities are not charged in two live sets. Rank-dependent maxima including numeric referents give HB new/reverse273+19r, permute273+11s+19r, slice273+19s+19r as the operative phase table shows. RB also dominates failure packet+numeric17. No V/P/r² survivor term is concealed in H.

## 4. C: actual library producers and shared consumers

### Actual selected library lowering

Captured installed GHC9.14.1 Haddock source: `library-source/GHC.Internal.List.html` and text, with original path/hash. Unfused scanr is `scanr _ q0 []=[q0]; scanr f q0 (x:xs)=f x q:qs where qs@(q:_)=scanr f q0 xs`. `scanrFB f c = \x ~(r,est) -> (f x r,r c est)` uses the documented lazy pair; strictUncurryScanr selects its final pair. `drop 1` matches one cons and returns its tail. The artifact contains the phase-controlled scanr/scanrList rules and lazy-pair note. This closes C's previously uninspected library-definition qualification without changing the library.

Use a dominating logical expansion allowing both ordinary recipes and the fusion pair/selectors; this overreserves mutually alternative lowering objects, not two executed traversals. Per input axis: tag/branch/fields4 + tail recipe(code,f,q0,xs)4 + conversion/product/head recipes10 + cons2 + lazy pair3 + selectors6 + saved arguments/controls4 + dispatch/return4 =37≤40. The terminal/drop/setup schedule is26≤32. Demanding a suffix product is conversion1+suffix/reference inspections2+multiply1+numeric assignment2+force/return3=9≤12. For rank0 no scan cells/products; positive rank produces≤r+1 output cells, returns r, and demands exactly r−1 multiplications when nonempty values are required. Spine-only demand needs zero products. The discarded full-product head is never needed. Thus late-zero huge dimensions do not force irrelevant numeric products.

Retained per-axis upper inventory: returned list/reference2 + arithmetic recipes10 + pair3 + selectors6 + tail recipe4 + completed bounded product2 =27, fixed8. Scratch≤6r+16: pending arithmetic frames4r **plus** converted bounded operands2r. Production-only list activations are bounded constant frames in16; they return a head without recursively forcing the suffix. During nonempty suffix evaluation saved multiplication continuations can nest r deep, hence the explicit variable operand/frame allowance. Discarded prefix/recipes may die; reserving them remains conservative. No GHC allocation/layout claim follows.

Witness production15r+4 (A, overriding C's12r); dimension/value production32r+8 (A's47r excluding the already-separate15r stock production, overriding C23r); stride40r+32+12·[count>0]·j; CheckedLayout12; item48; reshape update16. Sum **87r+120+12·[count>0]·j**. Item48=24 possible field/reference assignments+24 item/tuple/case inspections; reshape16 covers old/new record accesses/control. Witness R9r+2 (four node slots, three recipe refs, two proxy/update slots), dimensions12r+4, strides27r+8, layout5, item24, reshape4 →**48r+47**. Producer scratch(4r+8)+(4r+12)+(6r+16)+4+24+12=**14r+76**. Each P component is a different object family, not duplicate shapeDimensions expansion.

Runtime does not inherit an unpaid first-force cost for the two signed kernel CAFs: a successful opaque map with V>0 and r>0 already ran an ordinary checked dot and forced both before it existed at runtime entry. If r=0 no dot term uses them; if V=0 no runtime kernel runs. Inline `negate (toInteger M)`/`toInteger M` in affineSignedResult remain charged **per helper invocation**, unlike those two named CAFs. The common fixed six-work machine-maximum initialization and B's six-work first signed-CAF allowance are different source computations, not duplicate charges.

### Descriptor substitutions

Length12r+24, dimensions comparison16r+24, strides10r+12+r×34=44r+12. Source order/length sentinels match I994–1077. Fixed descriptor wrapper/count/stored-count/access/offset/empty/extrema/interval/contiguity/do sum20+12+20+6+14+6+12+14+4+52=160. Therefore:

```
Dc(empty)=160+2(12r+24)+(16r+24)=232+40r
Dc(nonempty,no interval)=Dc(empty)+(44r+12)+(164r+71)=315+248r
Dc(nonempty,interval)=previous+(42r+49)+(40r+44)=408+330r
Ds(empty)=96+(12r+24)+34+(42r+28)=182+54r
Ds(nonempty)=previous+(44r+12)+(164r+71)+(42r+49)+(40r+44)=358+344r
```

The shared signed/extrema/zipper/contiguity helpers are substituted whole; e.g. B's164r extrema already includes three34 signed checks, so they are not added again.

Scratch: descriptor64 local/wrapper frames+16 length+16 dimensions+16 stride cursor+64 checked-arithmetic arena+(5r+8) zipper+32 contiguity =216+5r≤224+5r. Local64=checkLayout frame21+descriptor frame19+24 scalar/register slots. The arithmetic arena dominates one stride signed helper≤32 **or** extrema frame15+signed frame11+temporaries12+pair5+Either6=49≤64; these phases do not overlap. Shift uses48 local(frame17+base references8+registers23)+32 signed-check arena+16 stride cursor+(11r+16) normalize frames+64 extrema+(5r+8) zipper+32 contiguity=216+16r≤224+16r. Dimension-copy scratch7r+8; returned numeric lists4r are separate. The signed pool is not simultaneously reused by multiple active checks.

### Bind

C fixed192 includes publication10 already in the common transaction: use182. Call sum:

`182+P(b)+ (39b+18)+(39r+18)+Dc(b,interval)+Ds(r)`

=752+166b+93r +[B>0](176+290b+12j(b))+[V>0](176+290r). Since j(b)≤b, this is≤**1104+468b+383r**. There is no registry or payload loop.

R=P_R(b)+8r+45 result/layout/report/state fields+2 shifted-offset scalar =94+48b+8r. H local58 (26 references+12 controls+12 saved arguments+8 scalars; lock/handler six removed from C) +P_H(b)+(7b+8)+4b+(7r+8)+(224+5b)+(224+16r) =598+30b+23r. Every concurrent dimension, producer and shifted-layout family is separately reserved.

### Pullback

Actual base/view producer debt is discharged by successful binding. Rechecks remain. Independent seed has one P(r), including caller-only retained output. Canonical gradient strides cost12b+8 empty; nonempty27b rounded32b+12; output4b+4, saved frames/pairs7b+16. Unlike scanr/drop, all b nonempty span products are forced. The empty branch builds zeros directly.

Initialization: zero step2 bound/branch+1 conversion+2 pointer/poke+1 increment+3 registers+3 IO controls=12. Scatter excluding kernels2 bound+6 checked interfaces+13 call setup+6 conversion/pointer/peek/poke+5 registers/loop=32. Fixed setup37≤40. Thus40+12B+V(32+2(104+96r)). No seed or destination list; one kernel workspace at a time.

Staging144=8 mask+20 pending inspection+20 allocation shell+20 initializer shell+12 advance+12 reverse singleton+8 fresh force+20 prepare shell+24 handler roots. Allocator execution excluded. C preparation334 loses publication12 already assigned to F, leaving322. Registry append/force **24n+24 once**. Fixed successful shell322+144+40+24=530≤536. Finite cleanup C90 loses rollback8 assigned to F:82≤88. One coordinate diagnostic39 is separate:88+39=127≤128. No success-result slack is borrowed for it.

Full body call sum is

`536+24n+P_W(r)+(39b+18)+(39r+18)+Dc(b,interval)+2Dc(r,no interval)+2[V>0](42r+49)+G_W(b)+12B+32V+2V(104+96r)+128`.

Before uniform branch domination it is1524+91b+206r+24n+[B>0](180+310b)+[V>0](264+500r+12j(r))+12B+32V+2V(104+96r). Taking indicators≤1 and j≤r gives **1968+401b+718r+12B+V(240+192r)+24n**. The fixed1524 includes the128 failure allowance (the corresponding successful constant is1396); the algebra is independently evaluated from the call sum, not native reports.

R=P_R(r)+4b+(4b+4)+4(n+1)+46 fixed gradient/ID/owner/memory/report/state/result fields+12 new byte/scalar-work/four updated-counter numeric slots =113+8b+48r+4n. A already owns charge/usage numeric referents. R's diagnostic alternative covers any finite failure without publishing candidate registry/result objects.

H=128 local/staging/prepared/cleanup +(14r+76)+(7b+8)+(7r+8)+4r +(224+5b)+2(224+5r)+2(5r+8)+(7b+16)+(64+4r)+4n =988+19b+49r+4n. The fixed128 is48 locals+32 frame/args+9 staged/fresh/request+19 initializer/prepared+20 numeric/diagnostic/cleanup slots. Both zippers and view dimension copy coexist with a single kernel. Registry frames4n and registry R4(n+1) are separate; no second24n+24 is charged.

## 5. Failure, provenance and proof limits

Every pre-body exit is bounded by A with its actual admitted e; post-body checked failures are source prefixes of the fully reserved row plus the once-priced diagnostic/finite cleanup. Numeric values retained in errors are assigned to the result/diagnostic identity set, not left in transient lists. For safe public maps, parent strides have admitted rank and bounded completed coefficients; this is why a reverse suffix cannot be an arbitrarily long privately forged list. Private fault checks diagnose representable metadata inconsistencies, not unsafe pointer provenance or arbitrary supplied thunk execution.

No field identity/report equality/demand counter establishes a W/H/R theorem. The theorem comes from the schedules and simultaneous identity inventories above. Python verifies the displayed sums, saturation/order algebra and independent fixtures; native O0/O2 controls verify scratch formula/debit/error/demand behavior. The executed equations-FINAL.py retains1784/390 fixed-subtotal regression assertions from its formula revision; they are not the final complete fixed proof. schedule_checks_final.py independently checks the complete1872/420/113 fixed work/transient/body-survivor inventories, including private-prepare forwarding and optional ownership completion. The operative coefficients do not change because these complete sums fit2048/448/128. Library source confirms the chosen logical scanr/drop expansion, not physical closure layouts. The frozen/current implementation delta remains unvalidated as an implementation; independent design review must scrutinize this candidate before any adoption authority.
