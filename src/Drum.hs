module Drum where

import Types
import Data.Ratio
import Control.Lens
import Control.Monad

volume :: Rational
volume = 140

note :: Rational -> Song -> Song
note n = cmap (\h -> h & dur .~ 4 / n)

dot :: Song -> Song
dot = cmap (\h -> h & dur %~ (* 1.5))

n1, n2, n4, n8, n16, n32, n64 :: Song -> Song
n1  = note 1
n2  = note 2
n4  = note 4
n8  = note 8
n16 = note 16
n32 = note 32
n64 = note 64

loop :: Song -> Song
loop s = s >> loop s

song :: Hit -> Song
song h = Composition (Single h, ())

velocity :: Rational -> Song -> Song
velocity v = cmap (\h -> h & vol .~ max 0 (min 127 v))

bd :: Song
bd = n4 $ song (Hit BassDrum1 0 volume)

sn :: Song
sn = n4 $ song (Hit SnareDrum2 0 volume)

hi :: Song
hi = n4 $ song (Hit ClosedHihat 0 volume)

rt :: Song
rt = n4 $ song (Hit BassDrum1 0 0)

dseq :: Sound -> Rational -> String -> Song
dseq s n cs = zipWithM_ velocity vs ts
  where
    vs = map toVol (filter (`elem` ".0123456789") cs)
    ts = repeat $ note n (song $ Hit s 0 0)

toVol :: Char -> Rational
toVol '.' = 0
toVol '0' = 10
toVol '1' = 20
toVol '2' = 35
toVol '3' = 50
toVol '4' = 66
toVol '5' = 80
toVol '6' = 92
toVol '7' = 104
toVol '8' = 116
toVol '9' = 127
toVol  _  = 0
