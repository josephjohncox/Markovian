module ExactSupportRoles where

import Data.Coerce (coerce)
import Markovian.Backend.Neural.Bridge.ExactSupportMask (
    ActionOutputLayout,
    ExactSupportMask,
 )
import Markovian.Compile.Exact (FiniteActionIndex)
import Markovian.MDP (ActionId)

newtype LeftAction = LeftAction Int
newtype RightAction = RightAction Int

breakActionId :: ActionId LeftAction -> ActionId RightAction
breakActionId = coerce

breakFiniteActionIndex :: FiniteActionIndex LeftAction -> FiniteActionIndex RightAction
breakFiniteActionIndex = coerce

breakActionOutputLayout :: ActionOutputLayout LeftAction -> ActionOutputLayout RightAction
breakActionOutputLayout = coerce

breakExactSupportMask :: ExactSupportMask LeftAction -> ExactSupportMask RightAction
breakExactSupportMask = coerce
