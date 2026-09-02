module ReverseProgramTypes where

import Markovian.Reverse
import Markovian.Reverse.Program

data Primitive parameter parameterCotangent input inputCotangent output outputCotangent

type Program parameter parameterCotangent input inputCotangent output outputCotangent =
    ReverseProgram Primitive () Rational parameter parameterCotangent input inputCotangent output outputCotangent

rationalIdentity :: Program () () Rational Rational Rational Rational
rationalIdentity = identityProgram rationalPrimal rationalCotangent

boolIdentity :: Program () () Bool Bool Bool Bool
boolIdentity = identityProgram boolPrimal boolCotangent

badIntermediateTypes :: Program ((), ()) ((), ()) Rational Rational Bool Bool
badIntermediateTypes = composeProgram rationalIdentity boolIdentity

badParameterProduct :: Program () () Rational Rational Rational Rational
badParameterProduct = composeProgram rationalIdentity rationalIdentity

badTapeProgramMismatch ::
    ReverseTape () Rational () () Bool Bool Bool Bool ->
    Either (ReverseProgramError ()) ((), Bool)
badTapeProgramMismatch tape = applyReverseTape tape (0 :: Rational)

rationalPrimal :: FinitePrimalSpace () Rational
rationalPrimal = finitePrimalSpace unitFiniteLayout (const (Right ())) (==) ExactCotangentEquality

boolPrimal :: FinitePrimalSpace () Bool
boolPrimal = finitePrimalSpace unitFiniteLayout (const (Right ())) (==) ExactCotangentEquality

rationalCotangent :: CotangentSpace () Rational Rational
rationalCotangent = cotangentSpace 0 (\left right -> Right (left + right)) (\scalar value -> Right (scalar * value)) (==) ExactCotangentEquality

boolCotangent :: CotangentSpace () Rational Bool
boolCotangent = cotangentSpace False (\left right -> Right (left || right)) (\_ value -> Right value) (==) ExactCotangentEquality
