module Example where
  
import Data.Monoid

import Types
import Interpret
import Drum
import Play

beat1 = mconcat [ mkComposition [hi, hi, hi, hi],
                  mkComposition [bd, sn, bd, sn] ]
                
bs = mkComposition $ cycle [bd, sn, bd, sn, bd, n8 sn, dot bd, sn]

h8 = mkComposition $ replicate 8 (n8 hi)

h12 = mkComposition $ replicate 12 (n8 hi)

trill = mkComposition $ replicate 8 (n16 hi)

hats = h8 <|> trill <|> h12 <|> trill <|> (mkComposition [n4 hi, hi, hi])

trap = mconcat [ hats,
                 mkComposition [n1 bd, n1 sn, n1 bd, n1 sn]]
                  
house = mconcat [ mkComposition $ cycle [dot $ n8 rt, n4 hi]
                , mkComposition $ cycle [n8 rt, n4 hi, n4 hi, n4 hi, n4 hi]
                , mkComposition $ cycle [n4 bd, n4 bd, n4 bd, n4 bd]]
