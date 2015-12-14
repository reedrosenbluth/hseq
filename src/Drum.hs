module Drum where

import Types
import Control.Lens

-- | default volume
volume :: Rational
volume = 127

-- | set the duration for each `Hit` in a `Song`
-- where the first argument represents the duration as a
-- fraction of a whole note.
note :: Rational -> Song -> Song
note n = cmap (\h -> h & dur .~ 4 / n)

-- | replicate a song a number of times
clone :: Rational -> Song -> Song
clone 1 s = s
clone n s = s >> clone (n-1) s

-- | modify the duration for each `Hit` in a `Song` to be
-- 1.5 times as long
dot :: Song -> Song
dot = cmap (\h -> h & dur %~ (* 1.5))

-- | quarter note rest
rest :: Rational -> Song
rest n = note n $ hsong (Hit BassDrum1 0 0)

-- | notes of various durations
n1, n2, n4, n8, n16, n32, n64 :: Song -> Song
n1  = note 1
n2  = note 2
n4  = note 4
n8  = note 8
n16 = note 16
n32 = note 32
n64 = note 64

-- | rests of various durations
r1, r2, r4, r8, r16, r32, r64 :: Song
r1  = rest 1
r2  = rest 2
r4  = rest 4
r8  = rest 8
r16 = rest 16
r32 = rest 32
r64 = rest 64


-- | create an infinite loop of a `Song`
loop :: Song -> Song
loop s = s >> loop s

-- | transform a `Beat` to a `Song`
song :: Beat -> Song
song b = Composition (b, ())

-- | transform a `Hit` to a `Song`
hsong :: Hit -> Song
hsong = song . Single

-- | change the velocity (volume) for each `Hit` in a `Song`
velocity :: Rational -> Song -> Song
velocity v = cmap (\h -> h & vol .~ max 0 (min 127 v))

-- | bass drum `Hit`
bd :: Song
bd = n4 $ hsong (Hit BassDrum1 0 volume)

-- | snare drum `Hit`
sn :: Song
sn = n4 $ hsong (Hit SnareDrum2 0 volume)

-- | hihat drum `Hit`
hi :: Song
hi = n4 $ hsong (Hit ClosedHihat 0 volume)

-- | rest `Hit`
rt :: Song
rt = n4 $ hsong (Hit BassDrum1 0 0)
