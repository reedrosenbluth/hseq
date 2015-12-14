module Interpret where

import Types
import Control.Lens

-- | calculates the total duration of a `Beat`
totalDur :: Beat -> Rational
totalDur (Single hit)     = hit ^. dur
totalDur (Series c1 c2)   = totalDur c1 + totalDur c2
totalDur (Parallel c1 c2) = max (totalDur c1) (totalDur c2)
totalDur None             = 0

-- | merge two sorted lists of `Hit`s
mergeHits :: [Hit] -> [Hit] -> [Hit]
mergeHits [] ys = ys
mergeHits xs [] = xs
mergeHits (x:xs) (y:ys)
  | x <= y    = x : mergeHits xs (y:ys)
  | otherwise = y : mergeHits (x:xs) ys

-- | apply a tempo to a composition
applyTempo :: Rational -> Composition a -> Composition a
applyTempo n = cmap (\h -> h & dur *~ (60000 / n))

-- | convert a `Composition` to a list of `Hit`s
toHits :: Composition a -> [Hit]
toHits (Composition (beat, _)) = go 0 beat
  where
    go d (Single hit)     = [hit & dur .~ d]
    go d (Series b1 b2)   = go d b1 ++ go (d + totalDur b1) b2
    go d (Parallel b1 b2) = mergeHits (go d b1) (go d b2)
    go _ None             = []

-- | interpret a `Composition` with a given tempo
interpret :: Rational -> Composition a -> [Hit]
interpret n c = toHits $ applyTempo n c
