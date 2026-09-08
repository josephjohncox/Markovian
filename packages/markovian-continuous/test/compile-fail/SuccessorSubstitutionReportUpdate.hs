module SuccessorSubstitutionReportUpdate where

import Markovian.Continuous.Kernel.JointAffine.Exact

badSubstitutionReportUpdate :: ExactSuccessorSubstitutionReport -> ExactSuccessorSubstitutionReport
badSubstitutionReportUpdate report = report{substitutionMaximumRationalBits = 1}
