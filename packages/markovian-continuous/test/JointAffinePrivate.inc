
-- Test-only declarations appended to an unchanged source copy by the boundary
-- harness. GHC's expression context can see private definitions without any
-- installed export, constructor, owner accessor, or callback being added.
privateSuccessorTests :: IO ()
privateSuccessorTests = do
    let limits = exactJointAffineLimits 100 100 2 1000 100000 1024
        subLimits = SuccessorSubstitutionLimits limits
        unit = RationalInterval 0 1
        wide = RationalInterval 0 2
        row n r s = (NoiseOwner n, unit, r, s)
        build r d s f rows = privateRight (exactJointAffineKernel limits (UnsafeExactAffineInputCoordinate r d) (UnsafeExactAffineInputCoordinate s f) rows)
        shared = sharedSuccessorOwners [(NoiseOwner 2,NoiseOwner 1)]
    left <- build 0 0 0 0 [row 1 0 (3/2)]
    right <- build 2 0 0 0 [row 2 (-3) 0]
    canceled <- privateRight (substituteLeftSuccessor subLimits left right shared)
    let SealedSuccessorSubstitution (UnsafeExactJointAffineKernel _ _ declarations executable raw owners coefficients projection) reservations report = canceled
    privateAssert "complete canceled declaration values/law" (map privateRow declarations == [(1,unit,0,0)])
    privateAssert "canceled executable view empty" (null executable)
    privateAssert "shared reservation-only original name retained without fake law" (reservations == [1,2] && raw == 1 && owners == 1 && coefficients == 6)
    privateAssert "cached projection history equals complete report" (jointAffineMaximumRationalBits projection == substitutionMaximumRationalBits report)
    zeroLeft <- build 0 0 0 0 [row 8 0 0,row 1 0 0]
    zeroRight <- build 0 0 0 0 [(NoiseOwner 1,wide,0,0)]
    fresh <- privateRight (substituteLeftSuccessor subLimits zeroLeft zeroRight (freshSuccessorOwners [(NoiseOwner 1,9)]))
    let SealedSuccessorSubstitution (UnsafeExactJointAffineKernel _ _ freshDeclarations freshExecutable _ _ _ _) freshReservations _ = fresh
    privateAssert "fresh full canonical zero source table preserves BOTH actual laws" (map privateRow freshDeclarations == [(1,unit,0,0),(8,unit,0,0),(9,wide,0,0)])
    privateAssert "equal original spelling reserved only once" (freshReservations == [1,8,9] && null freshExecutable)
    renamed <- privateRight (alphaRenameJointAffineKernel limits [(NoiseOwner 8,NoiseOwner 3)] zeroLeft)
    let UnsafeExactJointAffineKernel _ _ renamedDeclarations renamedExecutable _ _ _ _ = renamed
    privateAssert "alpha retains actual zero identity and law" (map privateRow renamedDeclarations == [(1,unit,0,0),(3,unit,0,0)] && null renamedExecutable)
    scoped <- privateRight (reScopeJointAffineKernel limits [(NoiseOwner 1,NoiseOwner 11),(NoiseOwner 3,NoiseOwner 13)] renamed)
    let UnsafeExactJointAffineKernel _ _ scopedDeclarations _ _ _ _ _ = scoped
    privateAssert "rescope retains complete actual manifest" (map privateRow scopedDeclarations == [(11,unit,0,0),(13,unit,0,0)])
    -- Witnesses contain these actual operands and plan. Execution has no second
    -- operand argument, so equal counts/phantoms cannot substitute another pair.
    capturedLeft <- build 99 101 2 3 [row 1 0 4]
    capturedRight <- build 5 6 7 8 [row 2 9 10]
    let cl = captureOperand capturedLeft
        cr = captureOperand capturedRight
        plan = substitutionPlan 1 1 1
    meter <- privateRight (admitOperandRationals limits cl (Meter 0 0) >>= admitOperandRationals limits cr)
    resolvedSourceRow <- privateRight (resolveRightSource (operandRows cr) (1,(2,1)))
    resolvedShared <- privateRight (resolveSharedTarget (operandRows cl) (1,resolvedSourceRow))
    privateRight (checkSharedInterval (1,resolvedShared))
    let admitted = AdmittedSuccessorOperands limits plan cl cr [resolvedShared] meter
        sharedWitness = VerifiedSharedSuccessorOwners admitted
        VerifiedSharedSuccessorOwners (AdmittedSuccessorOperands _ capturedPlan actualL actualR actualMap _) = sharedWitness
    privateAssert "witness full left manifest and unused reward captured" (privateOperand actualL == (99,101,2,3,[(1,unit,0,4)],7))
    privateAssert "witness full right manifest captured" (privateOperand actualR == (5,6,7,8,[(2,unit,9,10)],4))
    privateAssert "witness resolved map and admitted ledger captured" ([(resolvedSource entry,resolvedTarget entry) | entry <- actualMap] == [(2,1)] && planWork capturedPlan == 77)
    privateAssert "witness contains resolved actual source and shared-law rows" (map privateResolved actualMap == [((2,unit,9,10),1,Just (1,unit,0,4))])
    used <- privateRight (executeSharedSuccessor sharedWitness)
    let SealedSuccessorSubstitution (UnsafeExactJointAffineKernel (UnsafeExactAffineInputCoordinate rs ro) (UnsafeExactAffineInputCoordinate ss so) usedRows _ _ _ _ _) _ _ = used
    privateAssert "execution consumes actual captured coordinates and rows" ((rs,ro,ss,so) == (10,21,14,29) && map privateRow usedRows == [(1,unit,29,38)])
    resolvedFresh <- privateRight (resolveRightSource (operandRows cr) (1,(2,3)))
    freshUsed <- privateRight (executeFreshSuccessor (VerifiedFreshSuccessorOwners (AdmittedSuccessorOperands limits plan cl cr [resolvedFresh] meter)))
    let SealedSuccessorSubstitution (UnsafeExactJointAffineKernel _ _ freshUsedRows _ _ _ _ _) freshUsedReservations _ = freshUsed
    privateAssert "fresh witness consumes actual pair and complete resolved map" (map privateRow freshUsedRows == [(1,unit,20,28),(3,unit,9,10)] && freshUsedReservations == [1,2,3])
    -- Forged cached scalar counts test otherwise unreachable machine overflow
    -- without allocating giant tables. Poisoned fields must remain untouched.
    let cap = machineMaximum
        forged n = UnsafeExactJointAffineKernel
            (error "coordinate forced before preflight") (error "coordinate forced before preflight")
            (error "manifest forced before preflight") (error "executable view forced before preflight")
            n n 4 (makeReport JointAffineConstruction n n 0 4 0 0 1)
        -- Kernel fields are strict: use existing finite coordinates/list heads
        -- and poison their tails/contents, not bottom at the outer constructor.
        counted n = UnsafeExactJointAffineKernel (UnsafeExactAffineInputCoordinate 0 0) (UnsafeExactAffineInputCoordinate 0 0)
            [error "manifest entry forced before preflight"] [error "view entry forced before preflight"]
            n n 4 (makeReport JointAffineConstruction n n 0 4 0 0 1)
        runCounts lim nl nr mapping = substituteLeftSuccessor (SuccessorSubstitutionLimits lim) (counted nl) (counted nr) (FreshSuccessorOwners mapping)
        allCaps = exactJointAffineLimits cap cap cap cap cap cap
        wrapped = SuccessorSubstitutionAdmission
    -- Even an operand thunk is not forced when configured limits fail.
    privateLeft (wrapped (InvalidJointAffineLimit JointAffineRawEntries 0))
        (substituteLeftSuccessor (exactSuccessorSubstitutionLimits 0 1 1 1 1 1) (forged 0) (forged 0) (FreshSuccessorOwners (error "request inspected")))
    privateLeft (wrapped (JointAffineMachineIndexExceeded JointAffineRawEntries (2*cap))) (runCounts allCaps cap cap (error "spine inspected before raw machine check"))
    privateLeft (wrapped (JointAffineMachineIndexExceeded JointAffineCoefficients (12+4*cap))) (runCounts allCaps cap 0 [])
    let n = 2^(32 :: Int)
        hugeWork = privateStructuralInteger n 0 0 + 6 + 4*toInteger n
    privateLeft (wrapped (JointAffineMachineIndexExceeded JointAffineWork (fromInteger hugeWork))) (runCounts allCaps n 0 [])
    -- Computed machine errors precede smaller configured limits too.
    privateLeft (wrapped (JointAffineMachineIndexExceeded JointAffineCoefficients (12+4*cap))) (runCounts (exactJointAffineLimits cap cap 2 1 cap 1) cap 0 [])
    privateLeft (wrapped (JointAffineMachineIndexExceeded JointAffineWork (fromInteger hugeWork))) (runCounts (exactJointAffineLimits cap cap 2 cap 1 1) n 0 [])
    mapM_ (\nl -> mapM_ (\nr -> mapM_ (\m -> do
        let p = substitutionPlan nl nr m
        privateAssert "independent Integer phase-sum structural plan" (toInteger (planStructural p) == privateStructuralInteger nl nr m)
        privateAssert "independent arithmetic slot list" (planArithmetic p == naturalLength (replicate 6 () ++ replicate (fromIntegral (2*nl)) () ++ replicate (fromIntegral (2*(nl+nr))) ()))) [0..4]) [0..4]) [0..4]
    -- Malformed private inputs isolate rational/interval ordering that opaque
    -- public construction prevents. Full work still wins before their contents.
    let malformed bits r d s f rows = UnsafeExactJointAffineKernel (UnsafeExactAffineInputCoordinate r d) (UnsafeExactAffineInputCoordinate s f)
            rows (filter executableRow rows) 1 1 6 (makeReport JointAffineConstruction 1 1 0 6 2 0 bits)
        smallLimits = exactSuccessorSubstitutionLimits 3 3 2 20 77 2
        leftCached = malformed 5 0 0 0 0 [OwnerRow 1 unit 0 0]
        rightCached = malformed 6 0 0 0 0 [OwnerRow 2 unit 0 0]
        oneLeft = malformed 1 0 0 0 0 [OwnerRow 1 unit 0 0]
        oneRight = malformed 1 0 0 0 0 [OwnerRow 2 unit 0 0]
        badInterval = RationalInterval 16 16
    privateLeft (wrapped (JointAffineLimitExceeded JointAffineRationalBits 2 5)) (substituteLeftSuccessor smallLimits leftCached rightCached shared)
    privateLeft (wrapped (JointAffineLimitExceeded JointAffineRationalBits 2 5)) (substituteLeftSuccessor smallLimits (malformed 1 16 32 64 128 [OwnerRow 1 badInterval 256 512]) rightCached shared)
    privateLeft (wrapped (JointAffineInvalidInterval 16 16)) (substituteLeftSuccessor smallLimits (malformed 1 0 0 0 0 [OwnerRow 1 badInterval 256 512]) rightCached shared)
    privateLeft (wrapped (JointAffineLimitExceeded JointAffineRationalBits 2 5)) (substituteLeftSuccessor smallLimits (malformed 1 0 0 0 0 [OwnerRow 1 (RationalInterval 16 32) 64 128]) rightCached shared)
    privateLeft (wrapped (JointAffineLimitExceeded JointAffineRationalBits 2 6)) (substituteLeftSuccessor smallLimits (malformed 1 0 0 0 0 [OwnerRow 1 (RationalInterval 0 32) 64 128]) rightCached shared)
    privateLeft (wrapped (JointAffineLimitExceeded JointAffineRationalBits 2 7)) (substituteLeftSuccessor smallLimits (malformed 1 0 0 0 0 [OwnerRow 1 unit 64 128]) rightCached shared)
    privateLeft (wrapped (JointAffineLimitExceeded JointAffineRationalBits 2 8)) (substituteLeftSuccessor smallLimits (malformed 1 0 0 0 0 [OwnerRow 1 unit 0 128]) rightCached shared)
    privateLeft (wrapped (JointAffineLimitExceeded JointAffineRationalBits 2 6)) (substituteLeftSuccessor smallLimits oneLeft rightCached shared)
    privateLeft (wrapped (JointAffineLimitExceeded JointAffineRationalBits 2 5)) (substituteLeftSuccessor smallLimits oneLeft (malformed 1 16 32 64 128 [OwnerRow 2 wide 0 0]) shared)
    privateLeft (SuccessorSubstitutionSharedIntervalMismatch 1 unit wide) (substituteLeftSuccessor smallLimits oneLeft (malformed 1 0 0 0 0 [OwnerRow 2 wide 0 0]) shared)
    privateLeft (SuccessorSubstitutionUnknownSource 1) (substituteLeftSuccessor smallLimits leftCached oneRight (freshSuccessorOwners [(NoiseOwner 99,1)]))
    privateLeft (SuccessorSubstitutionFreshTargetCollision 1 SuccessorSubstitutionLeft 1) (substituteLeftSuccessor smallLimits leftCached oneRight (freshSuccessorOwners [(NoiseOwner 2,1)]))
    privateLeft (wrapped (JointAffineLimitExceeded JointAffineWork 76 77)) (substituteLeftSuccessor (exactSuccessorSubstitutionLimits 3 3 2 20 76 2) leftCached rightCached shared)
    privateLeft (JointAffineMachineIndexExceeded JointAffineWork (cap+1)) (operationPreflight allCaps 0 (cap+1))
    privateLeft (JointAffineMachineIndexExceeded JointAffineWork (cap+2)) (operationPreflight allCaps 1 cap)
    let bitThree = exactJointAffineLimits 1 1 2 6 100 3
    privateLeft (JointAffineLimitExceeded JointAffineRationalBits 3 4) (arithmetic bitThree (*) 8 0 (Meter 0 1))
    privateLeft (JointAffineLimitExceeded JointAffineRationalBits 3 4) (arithmetic bitThree (*) 0 8 (Meter 0 1))
    privateLeft (JointAffineLimitExceeded JointAffineRationalBits 3 4) (arithmetic bitThree (*) 3 3 (Meter 0 1))
    privateLeft (JointAffineLimitExceeded JointAffineRationalBits 3 4) (arithmetic bitThree (+) 7 1 (Meter 0 1))
    privateLeft (SuccessorSubstitutionAccountingMismatch 13 12)
        (executeSharedSuccessor (VerifiedSharedSuccessorOwners (AdmittedSuccessorOperands limits plan{planMultiplications = 7} cl cr [resolvedShared] meter)))
    privateLeft (SuccessorSubstitutionAccountingMismatch 12 13) (accounting 12 13)
    privateLeft (SuccessorSubstitutionAccountingMismatch 1 0) (checkCandidateIntervals [OwnerRow 1 unit 0 0,OwnerRow 1 wide 0 0])
    putStrLn "PASS: D079 private full-manifest, reservation, captured-operand witness, machine-count, rational-order and ledger invariants"

privateStructuralInteger :: Natural -> Natural -> Natural -> Integer
privateStructuralInteger left right mapping = sum
    [ l+l*l, r+r*r, 8+5*n, m, 2*m*m, 2*m*r, m*l, m*n, 2*m
    , n*m, n*n, n*n, n, h, h*h, h*h
    ]
  where
    l = toInteger left
    r = toInteger right
    m = toInteger mapping
    n = l+r
    h = n+m

privateRow :: OwnerRow -> (Natural,RationalInterval,Rational,Rational)
privateRow (OwnerRow owner interval reward successor) = (owner,interval,reward,successor)

privateResolved :: ResolvedSuccessorOwner -> ((Natural,RationalInterval,Rational,Rational),Natural,Maybe (Natural,RationalInterval,Rational,Rational))
privateResolved (ResolvedSuccessorOwner source target law) = (privateRow source,target,fmap privateRow law)

privateOperand :: SubstitutionOperand -> (Rational,Rational,Rational,Rational,[(Natural,RationalInterval,Rational,Rational)],Natural)
privateOperand (SubstitutionOperand r d s f rows bits) = (r,d,s,f,map privateRow rows,bits)

privateAssert :: String -> Bool -> IO ()
privateAssert label condition = unless condition (fail label)

privateRight :: (Show e) => Either e a -> IO a
privateRight = either (fail . show) pure

privateLeft :: (Eq e, Show e) => e -> Either e a -> IO ()
privateLeft expected result = case result of
    Left actual -> privateAssert ("expected " ++ show expected ++ "; actual " ++ show actual) (actual == expected)
    Right _ -> fail ("unexpected success, expected " ++ show expected)
