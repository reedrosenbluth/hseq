{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Types where

import Data.Monoid
import Data.Ratio
import Control.Applicative
import Control.Monad
import Control.Lens

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
   deriving (Show, Eq, Ord, Enum)

data Hit = Hit
    { _tone :: Sound
    , _dur  :: Rational
    , _vol  :: Rational
    } deriving (Show)

makeLenses ''Hit

data Beat =
    None
  | Single   Hit
  | Series   Beat Beat
  | Parallel Beat Beat
  deriving Show

data Composition a = Composition {unComp :: (Beat, a)} deriving Show

type Song = Composition ()

instance Functor Composition where
  fmap = liftM

instance Applicative Composition where
  pure  = return
  (<*>) = ap

instance Monad Composition where
  return a = Composition (None, a)
  Composition (b, a) >>= k =
    let (Composition (b', a')) = k a
    in  Composition (Series b b', a')

instance Monoid (Composition ()) where
  mempty = Composition (None, ())
  mappend (Composition (b1, _)) (Composition (b2, _))
    = Composition (Parallel b1 b2, ())

cmap :: (Hit -> Hit) -> Composition a -> Composition a
cmap f (Composition (c,a)) = Composition (hmap f c, a)
  where
    hmap g (Single h)       = Single (g h)
    hmap g (Series b1 b2)   = Series (hmap g b1) (hmap g b2)
    hmap g (Parallel b1 b2) = Parallel (hmap g b1) (hmap g b2)
    hmap _ b                = b
