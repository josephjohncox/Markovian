module MDPActionIdExport where

import Markovian.MDP.Exact (ActionId)

badActionOwner :: ActionId action
badActionOwner = error "ActionId must be imported from Markovian.Action"
