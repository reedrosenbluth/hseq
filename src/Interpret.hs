module Interpret where

import Types
import Control.Lens
import Data.Monoid

instance Monoid Composition where
  mempty  = Composition []
  mappend = merge

mkComposition :: [Hit] -> Composition
mkComposition comp = Composition $ zipWith replaceDur comp durs'
  where
    durs  = map (view dur) comp
    durs' = 0 : scanl1 (+) durs
    replaceDur h d = h & dur .~ d

merge :: Composition -> Composition -> Composition
merge (Composition as) (Composition bs) =
  Composition $ mergeHits as bs
  where
    mergeHits [] ys = ys
    mergeHits xs [] = xs
    mergeHits (a:xs) (b:ys) 
      | (a ^. dur) <= (b ^. dur) = a : mergeHits xs (b:ys)
      | otherwise = b : mergeHits (a:xs) ys
