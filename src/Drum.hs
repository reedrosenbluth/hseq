module Drum where

import Types
import Control.Lens

volume :: Rational
volume = 127

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

song :: Beat -> Song
song b = Composition (b, ())

hsong :: Hit -> Song
hsong = song . Single

velocity :: Rational -> Song -> Song
velocity v = cmap (\h -> h & vol .~ max 0 (min 127 v))

bd :: Song
bd = n4 $ hsong (Hit BassDrum1 0 volume)

sn :: Song
sn = n4 $ hsong (Hit SnareDrum2 0 volume)

hi :: Song
hi = n4 $ hsong (Hit ClosedHihat 0 volume)

rt :: Song
rt = n4 $ hsong (Hit BassDrum1 0 0)
