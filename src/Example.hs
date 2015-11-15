module Example where

import Types
import Play

bd :: Hit
bd = Hit BassDrum1 300 64

sn :: Hit
sn = Hit SnareDrum1 300 64

hi :: Hit
hi = Hit ClosedHihat 300 64



ex1 :: Composition
ex1 = [bd, sn, bd, hi, bd, sn, bd, hi]
