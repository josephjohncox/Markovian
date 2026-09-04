{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AutodiffQuoteBoundary where

import Markovian.Autodiff

badPathConstructor = PathHere SScalar
badQuoteConstructor = ProjectQuote (pathHere SScalar)
badReportConstructor = QuoteReport 1 1 1 1 1 1 1 1 1 1 1 1
badCompilationReportConstructor = QuoteCompilationReport undefined undefined
badLimitConstructor = QuotationLimitsValue 1 1 1 1 1 1 1 1 1 1 1 1

badLetCallback =
    letQuote
        (project (pathHere SScalar))
        (\_ -> project (pathHere SScalar))

badEscapedScope :: QuoteScope scope
badEscapedScope = withQuoteScope id

-- Equal runtime shapes do not make independently generated lexical scopes equal.
badCrossScope =
    withQuoteScope $ \(leftScope :: QuoteScope left) ->
        withQuoteScope $ \(_rightScope :: QuoteScope right) ->
            let environment = rootEnvironment SScalar
                leftPath = pathRight leftScope environment SScalar
             in (project leftPath :: Quote Rational 'Polynomial (BindEnvironment right (RootEnvironment 'Scalar) 'Scalar) 'NoParameters 'Scalar)

type WrongParameters = 'ParameterProduct 'NoParameters 'NoParameters

badParameterTree :: Either QuoteError (Program Rational 'Polynomial WrongParameters 'Scalar 'Scalar)
badParameterTree =
    withQuoteScope $ \scope ->
        lowerQuote
            (quotationLimits 20 20 20 20 20 20 20 20 20 20 20 20)
            (letQuote scope (project (pathHere SScalar)) (project (pathRight scope (rootEnvironment SScalar) SScalar)))
