{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

module Spec where

import Test.Hspec
import Test.QuickCheck

import Types

main :: IO ()
main = hspec $ do
  describe "associative property" $ do
    it "of series" $ property $
      \x y z -> (x >> y) >> z == x :: Song >> (y >> z)
    it "fail" $ 1 `shouldBe` (2 :: Int)
