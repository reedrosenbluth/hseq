module TypesSpec where

import Test.Hspec
import Test.QuickCheck

import Types
import Interpret
import Data.Monoid

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "associative property for series" $ do
    it "fail" $ 1 `shouldBe` (1 :: Int)
    it "of series" $
      property $
        \x y z -> toHits ((x >> y) >> z)
               == toHits ((x :: Song) >> ((y :: Song) >> (z :: Song)))

  describe "associative property for parallel" $ do
    it "fail" $ 1 `shouldBe` (1 :: Int)
    it "of series" $
      property $
        \x y z -> toHits ((x <> y) <> z)
               == toHits ((x :: Song) <> ((y :: Song) <> (z :: Song)))
