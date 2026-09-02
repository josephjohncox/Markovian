module MixedGameBoundary where

import Data.Coerce (coerce)
import Markovian.Game.Correlated.Exact
import Markovian.Game.Harsanyi.Exact
import Markovian.Game.NormalForm.Exact
import Markovian.Game.Outcome.Exact
import Markovian.Game.Profile.Finite
import Markovian.Game.Stochastic.Exact

newtype OtherOwner = OtherOwner String
newtype OtherAction = OtherAction Bool

badOwnedProductConstructor :: OwnedProduct owner choice
badOwnedProductConstructor = OwnedProduct undefined undefined undefined

badOwnedProfileConstructor :: OwnedProfile owner choice
badOwnedProfileConstructor = OwnedProfile undefined

badSimplexConstructor :: ExactSimplex choice
badSimplexConstructor = ExactSimplex undefined undefined

badNormalGameConstructor :: ExactNormalGame owner action
badNormalGameConstructor = ExactNormalGame undefined undefined

badMixedProfileConstructor :: ExactMixedProfile owner action
badMixedProfileConstructor = ExactMixedProfile undefined undefined

badCorrelationConstructor :: ExactCorrelationDevice owner action
badCorrelationConstructor = ExactCorrelationDevice undefined undefined

badOutcomeLawConstructor :: ExactOutcomeLaw outcome
badOutcomeLawConstructor = ExactOutcomeLaw undefined undefined

badStochasticGameConstructor :: ExactStochasticGame owner state action
badStochasticGameConstructor = ExactStochasticGame undefined undefined undefined undefined undefined

badTypePriorConstructor :: ExactTypePrior owner typ
badTypePriorConstructor = ExactTypePrior undefined undefined

badBehaviorConstructor :: ExactBehaviorProfile owner typ action
badBehaviorConstructor = ExactBehaviorProfile undefined undefined undefined

badHarsanyiConstructor :: ExactHarsanyiGame owner typ action
badHarsanyiConstructor = ExactHarsanyiGame undefined undefined undefined undefined

badContingentPlanConstructor :: ContingentPlan typ action
badContingentPlanConstructor = ContingentPlan undefined

badPlayerValuesOwnerCoerce :: ExactPlayerValues String -> ExactPlayerValues OtherOwner
badPlayerValuesOwnerCoerce = coerce

badNormalOwnerCoerce :: ExactNormalGame String Bool -> ExactNormalGame OtherOwner Bool
badNormalOwnerCoerce = coerce

badCorrelationOwnerCoerce :: ExactCorrelationDevice String Bool -> ExactCorrelationDevice OtherOwner Bool
badCorrelationOwnerCoerce = coerce

badPriorOwnerCoerce :: ExactTypePrior String Bool -> ExactTypePrior OtherOwner Bool
badPriorOwnerCoerce = coerce

badHarsanyiOwnerCoerce :: ExactHarsanyiGame String Bool Bool -> ExactHarsanyiGame OtherOwner Bool Bool
badHarsanyiOwnerCoerce = coerce

badContingentTypeCoerce :: ContingentPlan Bool Bool -> ContingentPlan OtherAction Bool
badContingentTypeCoerce = coerce

badOwnerCoerce :: ExactMixedProfile String Bool -> ExactMixedProfile OtherOwner Bool
badOwnerCoerce = coerce

badActionCoerce :: ExactMixedProfile String Bool -> ExactMixedProfile String OtherAction
badActionCoerce = coerce

badCorrelationAsIndependent :: ExactCorrelationDevice owner action -> ExactMixedProfile owner action
badCorrelationAsIndependent = id

badPriorAsStrategy :: ExactTypePrior owner typ -> ExactMixedProfile owner typ
badPriorAsStrategy = id
