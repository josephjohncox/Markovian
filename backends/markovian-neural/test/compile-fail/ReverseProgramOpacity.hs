module ReverseProgramOpacity where

import qualified Markovian.Backend.Neural.Reverse.Program as Program

badProgramConstructor = Program.PrimitiveProgram

badTapeConstructor = Program.StoredPrimitiveTape
