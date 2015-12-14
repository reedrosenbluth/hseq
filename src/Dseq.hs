module Dseq where

import Types
import Drum
import Control.Monad

-- | convert a string in the DSEQ format to a `Song` with a `Sound`
--   and a volume
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
