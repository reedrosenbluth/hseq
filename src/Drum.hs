module Drum where
  
import Types
import Control.Lens

volume :: Int
volume = 100

quarter :: Int
quarter = 280

n16 :: Hit -> Hit
n16 h = h & dur .~ round (fromIntegral quarter / 4)

n8 :: Hit -> Hit
n8 h = h & dur .~ round (fromIntegral quarter / 2)

n4 :: Hit -> Hit
n4 h = h & dur .~ quarter

n2 :: Hit -> Hit
n2 h = h & dur .~ (2 * quarter)

n1 :: Hit -> Hit
n1 h = h & dur .~ (4 * quarter)

dot :: Hit -> Hit
dot h = h & dur %~ (\d -> round (fromIntegral d * 1.5))

bd :: Hit
bd = n4 (Hit BassDrum1 0 volume)

sn :: Hit
sn = n4 (Hit SnareDrum2 0 volume)

hi :: Hit
hi = n4 (Hit ClosedHihat 0 volume)

rt :: Hit
rt = n4 (Hit BassDrum1 0 0)
