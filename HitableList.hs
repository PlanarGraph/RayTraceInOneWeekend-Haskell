{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HitableList where

import Hitable
import Ray

type HitableList = [Hitable]

hitL :: HitableList -> Ray -> Double -> Double -> Maybe HitRecord
hitL list r tMin tMax = fst $ foldl findClosest (Nothing, tMax) list
  where
    findClosest :: (Maybe HitRecord, Double) -> Hitable -> (Maybe HitRecord, Double)
    findClosest b@(hr, closest) hitable =
      let x = hit hitable r tMin closest in
          case x of
               Nothing -> b
               Just nhr -> (Just nhr, t nhr)
