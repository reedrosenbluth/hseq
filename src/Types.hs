{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Types where

import Data.Monoid
import Data.Ratio
import Control.Applicative
import Control.Monad
import Control.Lens hiding (elements)
import System.Random
import Test.QuickCheck

-- | Types of drum sounds
data Sound =
     BassDrum2     | BassDrum1     | SideStick    | SnareDrum1
   | HandClap      | SnareDrum2    | LowTom2      | ClosedHihat
   | LowTom1       | PedalHihat    | MidTom2      | OpenHihat
   | MidTom1       | HighTom2      | CrashCymbal1 | HighTom1
   | RideCymbal1   | ChineseCymbal | RideBell     | Tambourine
   | SplashCymbal  | Cowbell       | CrashCymbal2 | VibraSlap
   | RideCymbal2   | HighBongo     | LowBongo     | MuteHighConga
   | OpenHighConga | LowConga      | HighTimbale  | LowTimbale
   | HighAgogo     | LowAgogo      | Cabasa       | Maracas
   | ShortWhistle  | LongWhistle   | ShortGuiro   | LongGuiro
   | Claves        | HighWoodBlock | LowWoodBlock | MuteCuica
   | OpenCuica     | MuteTriangle  | OpenTriangle
   deriving (Show, Eq, Ord, Enum, Bounded)

instance Arbitrary Sound where
  arbitrary = toEnum <$> choose
    (fromEnum (minBound :: Sound),
     fromEnum (maxBound :: Sound))

-- | A drum `Hit` with a tone, duration, and volume
data Hit = Hit
    { _tone :: Sound
    , _dur  :: Rational
    , _vol  :: Rational
    } deriving (Show, Eq)

instance Arbitrary Hit where
  arbitrary = do
    tone <- arbitrary
    dur  <- toRational <$> choose (1 :: Int, 64)
    vol  <- toRational <$> choose (0 :: Int, 127)
    return $ Hit tone dur vol

makeLenses ''Hit

cmpToneVol :: Hit -> Hit -> Bool
cmpToneVol x y
  | xTone  < yTone = True
  | xTone == yTone = x ^. vol < y ^. vol
  | otherwise = False
  where xTone = x ^. tone
        yTone = y ^. tone

instance Ord Hit where
  x <= y
    | xDur  < yDur = True
    | xDur == yDur = cmpToneVol x y
    | otherwise    = False
    where xDur = x ^. dur
          yDur = y ^. dur

-- | Used for combining Hits and Beats
data Beat =
    None
  | Single   Hit
  | Series   Beat Beat
  | Parallel Beat Beat
  deriving (Show, Eq)

instance Arbitrary Beat where
  arbitrary = sized arbnB

arbnB :: Int -> Gen Beat
arbnB n = frequency [
    (1, return None),
    (3, liftM  Single arbitrary),
    (n, liftM2 Series (arbnB (n `div` 2)) (arbnB (n `div` 2))),
    (n, liftM2 Parallel (arbnB (n `div` 8)) (arbnB (n `div` 8))) ]

-- | We wrap a `Beat` in the `Composition` data structure in order
-- create a monad instance for it.
data Composition a = Composition (Beat, a) deriving (Show)

type Song = Composition ()

instance Arbitrary Song where
  arbitrary = do
    b <- arbitrary :: Gen Beat
    return $ Composition (b, ())

instance Functor Composition where
  fmap = liftM

instance Applicative Composition where
  pure  = return
  (<*>) = ap

-- | This is basically a specialized instance of the write monad
-- for composing compositions in series.
instance Monad Composition where
  return a = Composition (None, a)
  Composition (b, a) >>= k =
    let (Composition (b', a')) = k a
    in Composition (Series b b', a')

instance Semigroup (Composition ()) where
  (Composition (b1, _)) <> (Composition (b2, _))
    = Composition (Parallel b1 b2, ())

instance Monoid (Composition ()) where
  mempty = Composition (None, ())

-- | Lift a function on `Hit`s over a `Composition`
cmap :: (Hit -> Hit) -> Composition a -> Composition a
cmap f (Composition (c,a)) = Composition (hmap f c, a)
  where
    hmap g (Single h)       = Single (g h)
    hmap g (Series b1 b2)   = Series (hmap g b1) (hmap g b2)
    hmap g (Parallel b1 b2) = Parallel (hmap g b1) (hmap g b2)
    hmap _ b                = b
