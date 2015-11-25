module Example where

import Data.Monoid
import Control.Monad

import Types
import Drum
import Play

beat1 = mconcat [ sequence_ [hi, hi, hi, hi],
                  sequence_ [bd, sn, bd, sn] ]

h8    = replicateM_ 8 (n8 hi)
h12   = replicateM_ 12 (n8 hi)
trill = replicateM_ 8 (n16 hi)

hats = h8 >> trill >> h12 >> trill >> n4 hi >> n4 hi

trap = hats <> (n1 bd >> n1 sn >> n1 bd >> n1 sn)

wassup = n2 bd >> n8 bd >> n4 sn >> n2 bd >> n8 bd >> n4 bd >> n2 sn

icecube :: Song
icecube = measure BassDrum1   8 "7... .... 7... .... 7... .... 7.77 .7.."
       <> measure BassDrum2   8 ".... 7... .... 7... .... 7... .... 7..."
       <> measure SnareDrum2  8 ".... 4... .... 4... .... 4... .... 4..."
       <> measure ClosedHihat 8 "7.7. 7.77 .77. 7.77 7.7. 7.77 .77. ...."
       <> measure OpenHihat   8 ".... .... .... .... .... .... .... .7.."
