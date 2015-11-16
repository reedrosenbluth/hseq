{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens

data Sound = 
   BassDrum2
 | BassDrum1
 | SideStick
 | SnareDrum1
 | HandClap
 | SnareDrum2
 | LowTom2
 | ClosedHihat
 | LowTom1
 | PedalHihat
 | MidTom2
 | OpenHihat
 | MidTom1
 | HighTom2
 | CrashCymbal1
 | HighTom1
 | RideCymbal1
 | ChineseCymbal
 | RideBell
 | Tambourine
 | SplashCymbal
 | Cowbell
 | CrashCymbal2
 | VibraSlap
 | RideCymbal2
 | HighBongo
 | LowBongo
 | MuteHighConga
 | OpenHighConga
 | LowConga
 | HighTimbale
 | LowTimbale
 | HighAgogo
 | LowAgogo
 | Cabasa
 | Maracas
 | ShortWhistle
 | LongWhistle
 | ShortGuiro
 | LongGuiro
 | Claves
 | HighWoodBlock
 | LowWoodBlock
 | MuteCuica
 | OpenCuica
 | MuteTriangle
 | OpenTriangle
    deriving (Show,Eq,Ord,Enum)

data Hit = Hit
    { _tone :: Sound
    , _dur  :: Int
    , _vol  :: Int
    } deriving (Show)
    
makeLenses ''Hit

data Composition = Composition
    { run    :: [Hit]
    , endDur :: Int
    } deriving (Show)
