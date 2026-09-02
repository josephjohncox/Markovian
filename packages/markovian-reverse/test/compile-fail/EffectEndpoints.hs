module EffectEndpoints where

import Markovian.Reverse.Program
import Markovian.Reverse.Program.Effect

badEffectTapeSeed :: EffectReverseTape IO () Rational p pc x xc Bool Bool -> IO (Either (EffectReverseExecutionError ()) (pc, xc, EffectReverseExecutionReport))
badEffectTapeSeed tape = applyReverseTapeM tape (0 :: Rational)
