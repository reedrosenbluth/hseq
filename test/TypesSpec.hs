module TypesSpec where

import Test.Hspec
import Test.QuickCheck

import Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "associative property" $ do
    it "of series" $
      property $
        \x y z -> ((x >> y) >> z) == ((x :: Song) >> ((y :: Song) >> (z :: Song)))
    it "fail" $ 1 `shouldBe` (2 :: Int)
