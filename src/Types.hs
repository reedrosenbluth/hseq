{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens

data Instrument = Instrument
    | Rest
    | BassDrum
    | SnareDrum
    | HighHat
    | CrashCymbal

data Hit = Hit
    { _tone :: Instrument
    , _dur  :: Int
    , _vol  :: Double
    }

makeLenses ''Hit

type Composition = [Hit]
