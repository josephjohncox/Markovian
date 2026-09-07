module FeedbackRewardJVPOpacity where

import Markovian.Feedback.Value.Exact

-- Fully applied: exporting the constructor makes this binding compile.
badRewardJVPConstructor :: CheckedAffineRewardJVP String Int Int Int
badRewardJVPConstructor = UnsafeCheckedAffineRewardJVP undefined undefined undefined undefined
