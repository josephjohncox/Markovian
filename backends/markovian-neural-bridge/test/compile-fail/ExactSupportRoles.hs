module ExactSupportRoles where

import Data.Coerce (coerce)
import Markovian.Action (ActionId)
import Markovian.Backend.Neural.Bridge.DQN.Trainer (
    DQNEnvironmentStep,
    DQNTrainerError,
    DQNTrainerRun,
    DQNTrainerState,
 )
import Markovian.Backend.Neural.Bridge.ExactSupportMask (
    ActionOutputLayout,
    ExactSupportMask,
 )
import Markovian.Compile.Exact (FiniteActionIndex)

newtype LeftAction = LeftAction Int
newtype RightAction = RightAction Int

newtype LeftEnvironment = LeftEnvironment Int
newtype RightEnvironment = RightEnvironment Int

newtype LeftCallbackError = LeftCallbackError Int
newtype RightCallbackError = RightCallbackError Int

breakActionId :: ActionId LeftAction -> ActionId RightAction
breakActionId = coerce

breakFiniteActionIndex :: FiniteActionIndex LeftAction -> FiniteActionIndex RightAction
breakFiniteActionIndex = coerce

breakActionOutputLayout :: ActionOutputLayout LeftAction -> ActionOutputLayout RightAction
breakActionOutputLayout = coerce

breakExactSupportMask :: ExactSupportMask LeftAction -> ExactSupportMask RightAction
breakExactSupportMask = coerce

breakDQNTrainerState :: DQNTrainerState LeftEnvironment -> DQNTrainerState RightEnvironment
breakDQNTrainerState = coerce

breakDQNEnvironmentStep :: DQNEnvironmentStep LeftEnvironment -> DQNEnvironmentStep RightEnvironment
breakDQNEnvironmentStep = coerce

breakDQNTrainerError :: DQNTrainerError LeftCallbackError -> DQNTrainerError RightCallbackError
breakDQNTrainerError = coerce

breakDQNTrainerRun :: DQNTrainerRun LeftEnvironment LeftCallbackError -> DQNTrainerRun RightEnvironment RightCallbackError
breakDQNTrainerRun = coerce
