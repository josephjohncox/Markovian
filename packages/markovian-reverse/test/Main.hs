module Main (main) where

import Effect qualified
import Markovian.Reverse

main :: IO ()
main = case finiteLayout "scalar" 1 of
    Nothing -> fail "nonempty finite layout was rejected"
    Just witness -> case declaredCotangentSpace "scalar" witness (const (Right ())) (0 :: Rational) (\x y -> Right (x + y)) (\x y -> Right (x * y)) (==) ExactCotangentEquality of
        Nothing -> fail "declared cotangent was rejected"
        Just space -> do
            total <- either (const (fail "cotangent addition failed")) pure (addCotangents space 2 3)
            if finiteLayoutExtent witness == 1 && total == (5 :: Rational)
                then do
                    Effect.tests
                    putStrLn "markovian-reverse: focused boundary passed"
                else fail "reverse witness result changed"
