module Drum where

import Types
import Control.Lens
import Control.Monad

volume :: Int
volume = 100

quarter :: Int
quarter = 280

note :: Int -> Song -> Song
note n = cmap (\h -> h & dur .~ (round (fromIntegral quarter * 4) `div` n))

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

-- dot :: Hit -> Hit
-- dot h = h & dur %~ (\d -> round (fromIntegral d * 1.5))

song :: Hit -> Song
song h = Composition (Single h, ())

velocity :: Int -> Song -> Song
velocity v = cmap (\h -> h & vol .~ max 0 (min 127 v))

bd :: Song
bd = n4 $ song (Hit BassDrum1 0 volume)

sn :: Song
sn = n4 $ song (Hit SnareDrum2 0 volume)

hi :: Song
hi = n4 $ song (Hit ClosedHihat 0 volume)

rt :: Song
rt = n4 $ song (Hit BassDrum1 0 0)

measure :: Sound -> Int -> String -> Song
measure s n cs = zipWithM_ velocity vs ts
  where
    vs = map toVol (filter (`elem` ".0123456789") cs)
    ts = repeat $ note n (song $ Hit s 0 0)

toVol :: Char -> Int
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
