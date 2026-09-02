module ActionRoles where

import Data.Coerce (coerce)
import Markovian.Action (ActionId)

data LeftAction = LeftAction
newtype RightAction = RightAction LeftAction

breakActionId :: ActionId LeftAction -> ActionId RightAction
breakActionId = coerce
