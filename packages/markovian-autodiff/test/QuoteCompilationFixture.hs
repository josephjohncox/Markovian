{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Port of the frozen external Fixture.hs. Parameter association and mathematical
-- oracles are deliberately independent of lowering and compiler metadata.
module QuoteCompilationFixture (run) where

import Control.Monad (forM_, unless)
import Data.List (isPrefixOf, tails, transpose)
import Data.Ratio ((%))
import Markovian.Autodiff

type V2 = 'Vector 2
type I = 'Product V2 ('Product 'Scalar V2)
type O = 'Product ('Product 'Scalar 'Scalar) ('Product 'Scalar 'Scalar)
type Z = 'NoParameters
type PP = 'ParameterProduct
type Op p q = PP (PP p q) Z
type XP = PP Z Z
type SZP = PP Z (PP Z Z)
type CP p = PP p (PP (PP Z Z) Z)
type UP = Op (Owner "a" V2) XP
type VP = Op (CP Z) (Op Z (CP SZP))
type WP = Op (PP Z Z) (Op SZP Z)
type TP = Op (Op VP (CP XP)) (Op VP (CP XP))
type BP = PP (PP VP VP) (PP WP TP)
type FP = PP (PP Z UP) (PP (PP Z (Owner "b" 'Scalar)) (PP (PP Z (Owner "c" V2)) BP))

vec :: SShape V2
vec = SVector
inputShape :: SShape I
inputShape = SProduct vec (SProduct SScalar vec)
right :: (Show e) => Either e a -> a
right = either (error . show) id
v2 :: [Rational] -> Value Rational V2
v2 = right . vectorValue @2 2
coord :: Int -> Quote Rational 'Polynomial e p V2 -> Quote Rational 'Polynomial e (CP p) 'Scalar
coord j q = composeQuote q (compose (fanout (identity vec) (constantVector vec vec (v2 (if j == 0 then [1, 0] else [0, 1])))) (dot vec))
add, mul :: Quote Rational 'Polynomial e p 'Scalar -> Quote Rational 'Polynomial e q 'Scalar -> Quote Rational 'Polynomial e (Op p q) 'Scalar
add l r = composeQuote (fanoutQuote l r) addScalar
mul l r = composeQuote (fanoutQuote l r) multiplyScalar
fixture :: Quote Rational 'Polynomial (RootEnvironment I) FP O
fixture = withQuoteScope $ \su -> withQuoteScope $ \sb -> withQuoteScope $ \sc ->
    let e0 = rootEnvironment inputShape
        e1 = extendEnvironment su e0 vec
        e2 = extendEnvironment sb e1 SScalar
        sh1 = SProduct inputShape vec
        sh2 = SProduct sh1 SScalar
        root0 = project (pathHere inputShape)
        x0 = composeQuote root0 (first vec (SProduct SScalar vec))
        u = composeQuote (fanoutQuote (quoteProgramAt e0 (parameter @"a" inputShape vec)) x0) (hadamard vec)
        b = quoteProgramAt e1 (parameter @"b" sh1 SScalar)
        c = quoteProgramAt e2 (parameter @"c" sh2 vec)
        root = project (pathLeft sc (pathLeft sb (pathLeft su (pathHere inputShape) vec) SScalar) vec)
        readU = project (pathLeft sc (pathLeft sb (pathRight su e0 vec) SScalar) vec)
        readB = project (pathLeft sc (pathRight sb e1 SScalar) vec)
        x = composeQuote root (first vec (SProduct SScalar vec))
        s = composeQuote root (compose (second vec (SProduct SScalar vec)) (first SScalar vec))
        z = composeQuote root (compose (second vec (SProduct SScalar vec)) (second SScalar vec))
        v j = add (coord j readU) (mul readB (coord j z))
        w = add (composeQuote readU (sumVector vec)) (mul s readB)
        t = add (mul (v 0) (coord 0 x)) (mul (v 1) (coord 1 x))
     in letQuote su u (letQuote sb b (letQuote sc c (fanoutQuote (fanoutQuote (v 0) (v 1)) (fanoutQuote w t))))

-- A separately spelled parameter tree, not extracted from syntax or lowering.
zp :: ParameterValue Rational Z
zp = noParameters
pp :: ParameterValue Rational p -> ParameterValue Rational q -> ParameterValue Rational (PP p q)
pp = parameterProduct
op :: ParameterValue Rational p -> ParameterValue Rational q -> ParameterValue Rational (Op p q)
op p q = pp (pp p q) zp
xp :: ParameterValue Rational XP
xp = pp zp zp
szp :: ParameterValue Rational SZP
szp = pp zp (pp zp zp)
cp :: ParameterValue Rational p -> ParameterValue Rational (CP p)
cp p = pp p (pp (pp zp zp) zp)
vp :: ParameterValue Rational VP
vp = op (cp zp) (op zp (cp szp))
wp :: ParameterValue Rational WP
wp = op (pp zp zp) (op szp zp)
tp :: ParameterValue Rational TP
tp = op (op vp (cp xp)) (op vp (cp xp))
bp :: ParameterValue Rational BP
bp = pp (pp vp vp) (pp wp tp)
params :: [Rational] -> ParameterValue Rational FP
params [a0, a1, b, c0, c1] = pp (pp zp (op (ownedParameters (v2 [a0, a1])) xp)) (pp (pp zp (ownedParameters (scalarValue b))) (pp (pp zp (ownedParameters (v2 [c0, c1]))) bp))
params _ = error "parameter length"
input :: [Rational] -> Value Rational I
input [x0, x1, s, z0, z1] = productValue (v2 [x0, x1]) (productValue (scalarValue s) (v2 [z0, z1]))
input _ = error "input length"
output :: [Rational] -> Value Rational O
output [v0, v1, w, t] = productValue (productValue (scalarValue v0) (scalarValue v1)) (productValue (scalarValue w) (scalarValue t))
output _ = error "output length"
limits :: QuotationLimits
limits = quotationLimits 10000000 100000 10000 10000 100000 10000 100000 100000 10000000 10000000 100000000 256
check :: (Eq a, Show a) => String -> a -> a -> IO ()
check name actual expected = unless (actual == expected) (error (name ++ ": " ++ show actual ++ " /= " ++ show expected))

-- Independent scalar polynomials and formal Jacobian; no library derivative helpers.
primal :: [Rational] -> [Rational]
primal [a0, a1, b, _, _, x0, x1, s, z0, z1] = let u0 = a0 * x0; u1 = a1 * x1; v0 = u0 + b * z0; v1 = u1 + b * z1 in [v0, v1, u0 + u1 + s * b, v0 * x0 + v1 * x1]
primal _ = error "point length"
jacobian :: [Rational] -> [[Rational]]
jacobian [a0, a1, b, _, _, x0, x1, s, z0, z1] =
    [ [x0, 0, z0, 0, 0, a0, 0, 0, b, 0]
    , [0, x1, z1, 0, 0, 0, a1, 0, 0, b]
    , [x0, x1, s, 0, 0, a0, a1, b, 0, 0]
    , [x0 * x0, x1 * x1, z0 * x0 + z1 * x1, 0, 0, 2 * a0 * x0 + b * z0, 2 * a1 * x1 + b * z1, 0, b * x0, b * x1]
    ]
jacobian _ = error "point length"
pair :: [Rational] -> [Rational] -> Rational
pair xs ys
    | length xs == length ys = sum (zipWith (*) xs ys)
    | otherwise = error "pairing dimension mismatch"
basis :: Int -> Int -> [Rational]
basis n j = [if i == j then 1 else 0 | i <- [0 .. n - 1]]

-- Byte-for-byte baseline renderer oracle, including every primitive-use policy.
targetGolden :: TapePolicy -> String
targetGolden policy =
    unlines
        [ "autodiff-compile-report"
        , "semantics: exact-rational-formal-polynomial"
        , "tape-policy: " ++ show policy
        , "forward-scalar-work: 768"
        , "reverse-scalar-work: 932"
        , "double-comparison-policy: abs<=2.0e-10 or rel<=2.0e-8"
        , "target: markovian-reverse-owned-program"
        , "reverse-program-report"
        , "nodes: 163"
        , "primitives: 69"
        , "maximum-depth: 17"
        , "identity/composition/tensor/input-share/parameter-share: 13/51/0/30/0"
        , "primitive-tapes stored/recomputed: " ++ if policy == StorePullbacks then "69/0" else "0/69"
        , "owners: [\"a\",\"b\",\"c\"]"
        , "ownership-tree: ((unit * ((a:vector[2] * (unit * unit)) * unit)) * ((unit * b:scalar[1]) * ((unit * c:vector[2]) * (((((unit * ((unit * unit) * unit)) * ((unit * ((unit * (unit * unit)) * ((unit * unit) * unit))) * unit)) * unit) * (((unit * ((unit * unit) * unit)) * ((unit * ((unit * (unit * unit)) * ((unit * unit) * unit))) * unit)) * unit)) * ((((unit * unit) * (((unit * (unit * unit)) * unit) * unit)) * unit) * (((((((unit * ((unit * unit) * unit)) * ((unit * ((unit * (unit * unit)) * ((unit * unit) * unit))) * unit)) * unit) * ((unit * unit) * ((unit * unit) * unit))) * unit) * (((((unit * ((unit * unit) * unit)) * ((unit * ((unit * (unit * unit)) * ((unit * unit) * unit))) * unit)) * unit) * ((unit * unit) * ((unit * unit) * unit))) * unit)) * unit))))))"
        , "maximum-primal-extent: 10"
        , "maximum-cotangent-extent: 10"
        , "primitive-uses: " ++ show [name ++ "@1:" ++ mode ++ ":" ++ count | (name, count) <- [("parameter/a", "1"), ("quote-project", "18"), ("first", "4"), ("hadamard", "1"), ("parameter/b", "1"), ("parameter/c", "1"), ("constant-vector", "10"), ("dot", "10"), ("second", "9"), ("multiply-scalar", "7"), ("add-scalar", "6"), ("sum-vector", "1")]]
        ]
  where
    mode = if policy == StorePullbacks then "StoreCapturedPullback" else "RecomputePrimitive"

run :: IO ()
run = do
    let fixed = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
        points = [fixed, [0, -3, -2, 7, 11, -5, 0, 0, 2, -7], [2 % 3, -(3 % 5), 5 % 7, 7 % 11, 11 % 13, 13 % 17, -(17 % 19), 19 % 23, 23 % 29, -(29 % 31)]]
        (_, storedReport) = right (compileExactQuote limits StorePullbacks fixture)
    check "fixed primal" (primal fixed) [141, 196, 172, 5165]
    let parameterText = show (params (take 5 fixed))
        occurrences word = length (filter (isPrefixOf word) (tails parameterText))
    check "literal NoParameters leaves" (occurrences "noParameters") 79
    check "literal ParameterProduct constructors" (occurrences "parameterProduct") 81
    check "literal owner leaves" (occurrences "ownedParameters") 3
    check
        "fixed handwritten Jacobian"
        (jacobian fixed)
        [[13, 0, 23, 0, 0, 2, 0, 0, 5, 0], [0, 17, 29, 0, 0, 0, 3, 0, 0, 5], [13, 17, 19, 0, 0, 2, 3, 5, 0, 0], [169, 289, 792, 0, 0, 167, 247, 0, 65, 85]]
    forM_ [StorePullbacks, RecomputePullbacks] $ \policy -> do
        let (exe, report) = right (compileExactQuote limits policy fixture)
            target = quoteCompilationTarget report
            standalone = right (compileExactPolynomial (compilerLimits 163 69 17 69 10 10 1000 1000 932 256) policy (right (lowerQuote limits fixture)))
        check "actual target report agreement" target (exactCompileReport standalone)
        check "policy independent complete report" (quoteCompilationPreflight report) (quoteCompilationPreflight storedReport)
        check "complete target renderer golden" (renderCompileReport target) (targetGolden policy)
        forM_ points $ \point -> do
            let p = params (take 5 point)
                i = input (drop 5 point)
                expected = primal point
                rows = jacobian point
                direct = right (interpretExactQuote limits fixture p i)
                compiledRun = right (runExact exe p i)
                tape = exactRunTape compiledRun
                directions = map (basis 10) [0 .. 9] ++ [replicate 10 0, [1, -2, 3, -4, 5, -6, 7, -8, 9, -10]]
                seeds = map (basis 4) [0 .. 3] ++ [replicate 4 0, [2, -3, 5, -7]]
            check "parameter order" (parameterScalars p) (take 5 point)
            check "input order" (valueScalars i) (drop 5 point)
            check "direct primal" (valueScalars direct) expected
            check "compiled primal" (valueScalars (exactRunOutput compiledRun)) expected
            forM_ directions $ \direction -> do
                let (actualPrimal, tangent) = right (interpretExactQuoteJVP limits fixture p (params (take 5 direction)) i (input (drop 5 direction)))
                check "JVP primal" (valueScalars actualPrimal) expected
                check "JVP" (valueScalars tangent) (map (`pair` direction) rows)
                forM_ seeds $ \seed -> do
                    let (dp, di) = right (applyExactTape tape (output seed))
                        gradient = parameterScalars dp ++ valueScalars di
                        expectedGradient = map (pair seed) (transpose rows)
                    check "VJP" gradient expectedGradient
                    check "full gradient association" dp (params (take 5 expectedGradient))
                    check "pairing" (pair seed (valueScalars tangent)) (pair gradient direction)
    putStrLn "PASS: D080 full associated fixture, 72 directions / 432 retained-tape seeds at three Rational points"
