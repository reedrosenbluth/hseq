module Interpret where

import Types
import Control.Lens
import Data.Monoid

instance Monoid Composition where
  mempty  = Composition [] 0
  mappend = merge

lastHitDur :: [Hit] -> Int
lastHitDur []     = 0
lastHitDur [h]    = h ^. dur
lastHitDur (h:hs) = lastHitDur hs

mkComposition :: [Hit] -> Composition
mkComposition hits = Composition (zipWith replaceDur hits durs')
                                 (lastHitDur hits)
  where
    durs  = map (view dur) hits
    durs' = 0 : scanl1 (+) durs
    replaceDur h d = h & dur .~ d

merge :: Composition -> Composition -> Composition
merge (Composition as la) (Composition bs lb) =
  Composition (mergeHits as bs) (max la lb)
  where
    mergeHits [] ys = ys
    mergeHits xs [] = xs
    mergeHits (a:xs) (b:ys)
      | (a ^. dur) <= (b ^. dur) = a : mergeHits xs (b:ys)
      | otherwise = b : mergeHits (a:xs) ys

totalDur :: Composition -> Int
totalDur (Composition c _) = foldr (\a b -> (a ^. dur) + b) 0 c

infixr 6 <|>
(<|>) :: Composition -> Composition -> Composition
c1@(Composition as la) <|> (Composition bs lb) =
  Composition hs lb
  where
    bs' = map (over dur (+ tdur)) bs
    tdur = la + lastHitDur as
    hs = as ++ bs'
