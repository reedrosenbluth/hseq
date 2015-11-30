module Example where

import Data.Monoid
import Control.Monad

import Types
import Drum
import Play

beat1 = mconcat [ sequence_ [hi, hi, hi, hi],
                  sequence_ [bd, sn, bd, sn] ]


wassup = n2 bd >> n8 bd >> n4 sn >> n2 bd >> n8 bd >> n4 bd >> n2 sn

-- Trap Beat
-- Tempo: 230
h8    = replicateM_ 8 (n8 hi)
h12   = replicateM_ 12 (n8 hi)
trill = replicateM_ 8 (n16 hi)
hats = h8 >> trill >> h12 >> trill >> n4 hi >> n4 hi

trap = hats <> (n1 bd >> n1 sn >> n1 bd >> n1 sn)

-- Today Was A Good Day - Ice Cube
-- Tempo: 160
icecube :: Song
icecube = dseq BassDrum1   8 "7... .... 7... .... 7... .... 7.77 .7.."
       <> dseq BassDrum2   8 ".... 7... .... 7... .... 7... .... 7..."
       <> dseq SnareDrum2  8 ".... 4... .... 4... .... 4... .... 4..."
       <> dseq ClosedHihat 8 "7.7. 7.77 .77. 7.77 7.7. 7.77 .77. ...."
       <> dseq OpenHihat   8 ".... .... .... .... .... .... .... .7.."

-- House Beat
-- Tempo: 260
house = dseq MidTom1     8 "7... .7.. 7... 7..."
     <> dseq Claves      8 ".7.. .... .7.. ...."
     <> dseq ClosedHihat 8 "7... 7... 7... 7..."
     <> dseq BassDrum1   8 "..7. .... 7.7. ...."

hMoreHats = house     <> dseq ClosedHihat 8 "..7. ..7. ..7. ..7."
hAllHats  = hMoreHats <> dseq ClosedHihat 8 ".7.7 .7.7 .7.7 .7.7"

houseSong = sequence_ $ replicate 4 house
                     ++ replicate 4 hMoreHats
                     ++ replicate 4 hAllHats
                     ++ replicate 4 house

--
-- Famous Breakbeats
-- Tempo: 210
--

-- The Funky Drummer
funky = dseq OpenHihat   8 ".... ...7 .... .7.."
     <> dseq ClosedHihat 8 "7777 777. 7777 7.77"
     <> dseq SnareDrum1  8 ".... 7..7 .7.7 7..7"
     <> dseq BassDrum1   8 "7.7. ..7. ..7. .7.."

-- Impeach The President
impeach = dseq OpenHihat   8 ".... .... ..7. ...."
       <> dseq ClosedHihat 8 "7.7. 7.77 7... 7.7."
       <> dseq SnareDrum1  8 ".... 7... .... 7..."
       <> dseq BassDrum1   8 "7... ...7 7... ..7."

-- When The Levee Breaks
levee = dseq OpenHihat   8 "7.7. 7.7. 7.7. 7.7."
     <> dseq SnareDrum1  8 ".... 7... .... 7..."
     <> dseq BassDrum1   8 "77.. ...7 ..77 ...."

-- Cold Sweat
sweat = dseq RideCymbal1 8 "7.7. 7.7. 7.7. 7.7."
     <> dseq SnareDrum1  8 ".... 7..7 .... 7..7"
     <> dseq BassDrum1   8 "7... .... 7.7. ...."
