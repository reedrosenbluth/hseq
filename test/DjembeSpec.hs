module DjembeSpec where

import Test.Hspec
import Test.QuickCheck

import Types
import Drum
import Interpret
import Data.Monoid

main :: IO ()
main = hspec spec

instance Eq (Composition a) where
  x == y = toHits x == toHits y

getBeat :: Song -> Beat
getBeat (Composition (b, _)) = b

spec :: Spec
spec = do
  describe "associative property of songs" $ do
    it "of series" $
      property $
        \x y z -> ((x >> y) >> z)
               == ((x :: Song) >> ((y :: Song) >> (z :: Song)))

    it "of parallel" $
      property $
        \x y z -> ((x <> y) <> z)
               == (x :: Song) <> ((y :: Song) <> (z :: Song))

  describe "commutativity property of songs" $ do
    it "of parallel" $
      property $
        \x y -> (x <> y)
             == ((y :: Song) <> (x :: Song))

  describe "interpret" $ do
    it "totalDur" $
      property $
        \b -> if b == None then True else
                (totalDur $ getBeat $ applyTempo 50  $ song b) >
                (totalDur $ getBeat $ applyTempo 100 $ song b)

  describe "drum" $ do
    it "note" $
      n1 bd `shouldBe` hsong (Hit BassDrum1 4 volume)

    it "velocity" $
      velocity 100 bd `shouldBe` (n4 $ hsong (Hit BassDrum1 0 100))

    it "hsong" $
      hsong (Hit BassDrum1 0 volume) `shouldBe`
        (Composition (Single (Hit BassDrum1 0 volume), ()))
