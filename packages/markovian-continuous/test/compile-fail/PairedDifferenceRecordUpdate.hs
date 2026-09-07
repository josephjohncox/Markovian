module PairedDifferenceRecordUpdate where

import Markovian.Continuous.Measure.Exact

badPairedRecordUpdate :: PairedDifferenceReport -> PairedDifferenceReport
badPairedRecordUpdate report = report{pairedVarianceDifference = 0}
