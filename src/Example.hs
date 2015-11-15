module Example where
  
import Data.Monoid

import Types
import Interpret
import Play

bd :: Hit
bd = Hit BassDrum1 300 100

sn :: Hit
sn = Hit SnareDrum1 300 100

hi :: Hit
hi = Hit ClosedHihat 300 100

rt :: Hit
rt = Hit BassDrum1 300 0

ex1 :: Composition
ex1 = mkComposition [rt, hi, rt, hi]

ex2 :: Composition
ex2 = mkComposition [bd, rt, bd, rt]

ex3 :: Composition
ex3 = ex1 <> ex2
