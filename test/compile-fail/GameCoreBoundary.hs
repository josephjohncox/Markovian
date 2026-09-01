module GameCoreBoundary where

import Data.Coerce (coerce)
import Markovian.Game.Arena
import Markovian.Game.Strategy

badArenaConstructor :: Arena position move
badArenaConstructor = Arena undefined undefined undefined undefined

badHistoryConstructor :: LegalHistory position move
badHistoryConstructor = LegalHistory undefined undefined undefined

badPlayConstructor :: LegalPlay aPosition aMove bPosition bMove
badPlayConstructor = LegalPlay undefined undefined undefined undefined undefined

badStrategyConstructor :: Strategy aPosition aMove bPosition bMove
badStrategyConstructor = Strategy undefined undefined undefined

badArenaEndpointCoerce :: Arena Bool Bool -> Arena () Bool
badArenaEndpointCoerce = coerce

badArenaMoveCoerce :: Arena Bool Bool -> Arena Bool ()
badArenaMoveCoerce = coerce

badPlayDomainMoveCoerce ::
    LegalPlay Bool Bool Bool Bool ->
    LegalPlay Bool () Bool Bool
badPlayDomainMoveCoerce = coerce

badPlayCodomainPositionCoerce ::
    LegalPlay Bool Bool Bool Bool ->
    LegalPlay Bool Bool () Bool
badPlayCodomainPositionCoerce = coerce

badPlayCodomainMoveCoerce ::
    LegalPlay Bool Bool Bool Bool ->
    LegalPlay Bool Bool Bool ()
badPlayCodomainMoveCoerce = coerce

badStrategyEndpointCoerce ::
    Strategy Bool Bool Bool Bool ->
    Strategy () Bool Bool Bool
badStrategyEndpointCoerce = coerce

badStrategyMoveCoerce ::
    Strategy Bool Bool Bool Bool ->
    Strategy Bool () Bool Bool
badStrategyMoveCoerce = coerce

badStrategyCodomainCoerce ::
    Strategy Bool Bool Bool Bool ->
    Strategy Bool Bool () ()
badStrategyCodomainCoerce = coerce
