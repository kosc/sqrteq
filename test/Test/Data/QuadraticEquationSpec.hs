module Test.Data.QuadraticEquationSpec
    ( spec
    ) where

import           Test.Hspec             (Spec, describe, it, shouldBe)

import           Data.QuadraticEquation (QuadraticEquation (..), Solution (..),
                                         discriminant, solve)

spec :: Spec
spec = do
    describe "discriminant" $ do
        it "should be zero for equation with single solution" $ do
            discriminant (QuadraticEquation 1 6 9) `shouldBe` 0
        it "should be positive for equation with two solutions" $ do
            discriminant (QuadraticEquation 2 4 (-7)) `shouldBe` 72
        it "should be negative for equation without solutions" $ do
            discriminant (QuadraticEquation 2 4 7) `shouldBe` -40
    describe "solve" $ do
        it "should return single solution with zero discriminant" $ do
            solve (QuadraticEquation (1 :: Float) 6 9) `shouldBe` OneSolution (-3)
        it "should return two rots with positive discriminant" $ do
            solve (QuadraticEquation (1 :: Float) (-4) 0) `shouldBe` TwoSolutions 4 0
        it "should return no solutions with negative discriminant" $ do
            solve (QuadraticEquation (2 :: Float) 4 7) `shouldBe` NoSolutions
