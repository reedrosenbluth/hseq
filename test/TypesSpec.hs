module TypesSpec where

import Test.Hspec
import Test.QuickCheck

import Types
import Interpret
import Data.Monoid

main :: IO ()
main = hspec spec

instance Eq (Composition a) where
  x == y = toHits x == toHits y

spec :: Spec
spec = do
  describe "associative property" $
    it "of series" $
      property $
        \x y z -> ((x >> y) >> z)
               == ((x :: Song) >> ((y :: Song) >> (z :: Song)))

  describe "associative property" $
    it "of series" $
      property $
        \x y z -> ((x <> y) <> z)
               == (x :: Song) <> ((y :: Song) <> (z :: Song))

  describe "commutativity property" $
    it "of parallel" $
      property $
        \x y -> (x <> y)
             == ((y :: Song) <> (x :: Song))
