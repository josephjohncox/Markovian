{-# LANGUAGE RoleAnnotations #-}

-- | Stable nominal identities for finite-model actions.
module Markovian.Action (
    ActionId,
    actionId,
    actionValue,
) where

-- | The stable identity of an agent choice. It is not a stochastic outcome.
type role ActionId nominal

newtype ActionId action = ActionId action
    deriving (Eq, Ord, Show)

-- | Construct an action ID.
actionId :: action -> ActionId action
actionId = ActionId

-- | Read an action ID.
actionValue :: ActionId action -> action
actionValue (ActionId value) = value
