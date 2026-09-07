module FeedbackRewardJVPBoundary where

import Data.Coerce (coerce)
import Markovian.Feedback.Value.Exact

newtype Owner = Owner String
newtype Input = Input Int
newtype Loop = Loop Int
newtype Output = Output Int

badRewardJVPOwnerCoerce :: CheckedAffineRewardJVP String Int Int Int -> CheckedAffineRewardJVP Owner Int Int Int
badRewardJVPOwnerCoerce = coerce

badRewardJVPInputCoerce :: CheckedAffineRewardJVP String Int Int Int -> CheckedAffineRewardJVP String Input Int Int
badRewardJVPInputCoerce = coerce

badRewardJVPLoopCoerce :: CheckedAffineRewardJVP String Int Int Int -> CheckedAffineRewardJVP String Int Loop Int
badRewardJVPLoopCoerce = coerce

badRewardJVPOutputCoerce :: CheckedAffineRewardJVP String Int Int Int -> CheckedAffineRewardJVP String Int Int Output
badRewardJVPOutputCoerce = coerce
